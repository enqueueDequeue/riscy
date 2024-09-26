package io.riscy.stages

import chisel3.util.{Valid, isPow2, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Input, Mem, Module, Output, PrintableHelper, RegInit, UInt, Wire, assert, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.signals.{Parameters, ROBSignals}

class ROB()(implicit val params: Parameters) extends Module {
  val nROBEntries = params.nROBEntries
  val nIQEntries = params.nIQEntries
  val addrWidth = params.addrWidth

  require(isPow2(nROBEntries))

  val io = IO(new Bundle {
    // allocate
    val allocate = Input(Bool())
    val allocatedIdx = Output(Valid(UInt(log2Ceil(nROBEntries).W)))

    // instruction allocation
    val instSignals = Input(Valid(new Bundle {
      val pc = UInt(addrWidth.W)
      val data = new ROBSignals()
    }))

    // instruction dispatch
    val readRobIdx = Input(Valid(UInt(log2Ceil(nROBEntries).W)))
    val robData = Output(Valid(new Bundle {
      val pc = UInt(addrWidth.W)
      val data = new ROBSignals()
    }))

    // flush
    val flush = Input(Valid(new Bundle {
      val updatePc = Valid(UInt(addrWidth.W))
      val robIdx = UInt(log2Ceil(nROBEntries).W)
    }))

    // instruction commit
    val commitRobIdx0 = Input(Valid(UInt(log2Ceil(nROBEntries).W)))
    val commitRobIdx1 = Input(Valid(UInt(log2Ceil(nROBEntries).W)))

    // branch prediction check
    val predictionRobIdx = Input(Valid(UInt(log2Ceil(nROBEntries).W)))

    val prediction = Output(Valid(new Bundle {
      val robIdx = UInt(log2Ceil(nROBEntries).W)
      val pc = UInt(addrWidth.W)
    }))

    // instruction retire
    val retireInst = Output(Valid(new Bundle {
      val flush = Bool()
      val pc = UInt(addrWidth.W)
      val signals = new ROBSignals()
    }))

    // general
    val robHead = Output(UInt(log2Ceil(nROBEntries).W))
  })

  // everything between robHead and robTail are
  // active instructions in the pipeline
  // NOTE: Even though we allocate nROBEntries,
  //  we can only address (nROBEntries - 1) entries
  //  This is because, tail MUST trail the head by
  //  at least one entry
  val robHead = RegInit(0.U(log2Ceil(nROBEntries).W))
  val robTail = RegInit(0.U(log2Ceil(nROBEntries).W))
  val entries = Mem(nROBEntries, Valid(new Bundle {
    val committed = Bool()
    val flush = Bool()
    val pc = UInt(addrWidth.W)
    val data = new ROBSignals()
  }))

  val canAllocate = robHead =/= (robTail + 1.U)

  when(io.allocate && canAllocate) {
    printf(cf"ROB: Allocating: head: $robHead, tail: $robTail\n")

    io.allocatedIdx.valid := true.B
    io.allocatedIdx.bits := robTail

    entries(robTail).valid := false.B

    robTail := robTail + 1.U
  }.otherwise {
    printf(cf"ROB: Didn't allocate: head: $robHead, tail: $robTail, allocate: ${io.allocate}\n")

    io.allocatedIdx.valid := false.B
    io.allocatedIdx.bits := DontCare
  }

  when(io.instSignals.valid) {
    printf(cf"ROB: Storing instSignals: ${io.instSignals}\n")

    val robIdx = io.instSignals.bits.data.robIdx

    entries(robIdx).valid := true.B
    entries(robIdx).bits.committed := false.B
    entries(robIdx).bits.flush := false.B
    entries(robIdx).bits.pc := io.instSignals.bits.pc
    entries(robIdx).bits.data := io.instSignals.bits.data
  }

  when(io.readRobIdx.valid) {
    assert(entries(io.readRobIdx.bits).valid, "input robIdx invalid")
    assert(!entries(io.readRobIdx.bits).bits.committed, "Are you sure? you wanna read a committed instruction")

    io.robData.valid := true.B
    io.robData.bits.pc := entries(io.readRobIdx.bits).bits.pc
    io.robData.bits.data := entries(io.readRobIdx.bits).bits.data
  }.otherwise {
    io.robData.valid := false.B
    io.robData.bits := DontCare
  }

  when(io.commitRobIdx0.valid) {
    printf(cf"ROB: committing ${io.commitRobIdx0} @ commitRobIdx0\n")

    assert(entries(io.commitRobIdx0.bits).valid, "Invalid instruction being committed")
    assert(!entries(io.commitRobIdx0.bits).bits.committed, "Cannot commit already committed instruction")

    entries(io.commitRobIdx0.bits).bits.committed := true.B

    // NOTE: Head can be moved up by one position here itself
    // It would be advantageous too. A new instruction can be allocated
    // in the same cycle as this is being committed
    // Which is helpful in cases where the ROB is running full
    // Maybe this can be considered as an upgrade in the v2
  }

  when(io.commitRobIdx1.valid) {
    printf(cf"ROB: committing ${io.commitRobIdx1} @ commitRobIdx1\n")

    assert(entries(io.commitRobIdx1.bits).valid, "Invalid instruction being committed")
    assert(!entries(io.commitRobIdx1.bits).bits.committed, "Cannot commit already committed instruction")

    entries(io.commitRobIdx1.bits).bits.committed := true.B
  }

  // keep commiting the instructions in the case of O3
  when(entries(robHead).valid && entries(robHead).bits.committed && robHead =/= robTail) {
    printf(cf"ROB: retiring rob head: $robHead, tail: $robTail\n")

    val nextRobHead = robHead + 1.U

    entries(robHead).valid := false.B

    io.retireInst.valid := true.B
    io.retireInst.bits.pc := entries(robHead).bits.pc
    io.retireInst.bits.flush := entries(robHead).bits.flush
    io.retireInst.bits.signals := entries(robHead).bits.data

    when(entries(robHead).bits.flush) {
      printf(cf"ROB: flushing\n")
      robTail := nextRobHead
    }

    robHead := nextRobHead
  }.otherwise {
    io.retireInst.valid := false.B
    io.retireInst.bits := DontCare
  }

  when(io.flush.valid) {
    printf(cf"ROB: flushing: ${io.flush.bits}\n")

    assert(entries(io.flush.bits.robIdx).valid, "Flushing entry invalid")

    // NOTE: This operation will overwrite the program counter of the flushed instruction
    //       to the update program counter. And, the processor when looks at the retired
    //       instruction and find that it needs to be flushed, it will restart execution
    //       from the program counter of that instruction which will be the updated PC
    //       in this case.

    entries(io.flush.bits.robIdx).bits.flush := true.B

    when(io.flush.bits.updatePc.valid) {
      entries(io.flush.bits.robIdx).bits.pc := io.flush.bits.updatePc.bits
    }
  }

  when(io.predictionRobIdx.valid) {
    val nextRobIdx = io.predictionRobIdx.bits + 1.U

    io.prediction.valid := entries(nextRobIdx).valid
    io.prediction.bits.robIdx := nextRobIdx
    io.prediction.bits.pc := entries(nextRobIdx).bits.pc
  }.otherwise {
    io.prediction.valid := false.B
    io.prediction.bits := DontCare
  }

  io.robHead := robHead
}
