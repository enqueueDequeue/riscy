package io.riscy.stages

import chisel3.util.{Valid, isPow2, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Input, Mem, Module, Output, PrintableHelper, RegInit, UInt, assert, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.signals.{Parameters, ROBSignals}

class ROB()(implicit val params: Parameters) extends Module {
  val instWidth = params.instWidth
  val nROBEntries = params.nROBEntries
  val nIQEntries = params.nIQEntries

  require(isPow2(nROBEntries))

  val io = IO(new Bundle {
    // allocate
    val allocate = Input(Bool())
    val allocatedIdx = Output(Valid(UInt(log2Ceil(nROBEntries).W)))

    // instruction allocation
    val instSignals = Input(Valid(new Bundle {
      val pc = UInt(instWidth.W)
      val data = new ROBSignals()
    }))

    // instruction dispatch
    val readRobIdx = Input(Valid(UInt(log2Ceil(nROBEntries).W)))
    val robData = Output(Valid(new Bundle {
      val pc = UInt(instWidth.W)
      val data = new ROBSignals()
    }))

    // flush
    val flush = Input(Valid(UInt(log2Ceil(nROBEntries).W)))

    // instruction commit
    val commitRobIdx = Input(Valid(UInt(log2Ceil(nROBEntries).W)))

    // instruction retire
    val retireInst = Output(Valid(new ROBSignals()))
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
    val pc = UInt(instWidth.W)
    val data = new ROBSignals()
  }))

  val canAllocate = robHead =/= (robTail + 1.U)

  when(io.allocate && canAllocate) {
    printf(cf"Allocating: head: $robHead, tail: $robTail\n")

    io.allocatedIdx.valid := true.B
    io.allocatedIdx.bits := robTail

    robTail := robTail + 1.U
  }.otherwise {
    printf(cf"Didn't allocate: head: $robHead, tail: $robTail, allocate: ${io.allocate}\n")

    io.allocatedIdx.valid := false.B
    io.allocatedIdx.bits := DontCare
  }

  when(io.instSignals.valid) {
    printf(cf"Storing instSignals: ${io.instSignals}\n")

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

  when(io.commitRobIdx.valid) {
    assert(entries(io.commitRobIdx.bits).valid, "Invalid instruction being committed")
    assert(!entries(io.commitRobIdx.bits).bits.committed, "Cannot commit already committed instruction")

    entries(io.commitRobIdx.bits).bits.committed := true.B

    // NOTE: Head can be moved up by one position here itself
    // It would be advantageous too. A new instruction can be allocated
    // in the same cycle as this is being committed
    // Which is helpful in cases where the ROB is running full
    // Maybe this can be considered as an upgrade in the v2
  }

  // keep commiting the instructions in the case of O3
  when(entries(robHead).valid && entries(robHead).bits.committed) {
    val nextRobHead = robHead + 1.U

    entries(robHead).valid := false.B

    io.retireInst.valid := true.B
    io.retireInst.bits := entries(robHead).bits.data

    // todo: completely figure how to flush
    //  Need to copy the rrat to rat
    //  need to update the IQ with the right values
    //  (maybe mark all the entries as invalid?)
    //  All the ldq and stq entries are invalid?
    //  Update the target PC?

    when(entries(robHead).bits.flush) {
      robTail := nextRobHead
    }

    robHead := nextRobHead
  }.otherwise {
    io.retireInst.valid := false.B
    io.retireInst.bits := DontCare
  }

  when(io.flush.valid) {
    assert(entries(io.flush.bits).valid, "Flushing entry invalid")
    entries(io.flush.bits).bits.flush := true.B
  }
}
