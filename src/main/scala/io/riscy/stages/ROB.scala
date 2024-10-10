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

    // exception
    val exception = Input(Valid(new Bundle {
      // todo: may need a description of the exception?
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

  val entryValidList = Mem(nROBEntries, Bool())
  val entryCommittedList = Mem(nROBEntries, Bool())
  val entryFlushedList = Mem(nROBEntries, Bool())
  val entryExceptedList = Mem(nROBEntries, Bool())
  val entryPCList = Mem(nROBEntries, UInt(addrWidth.W))
  val entryDataList = Mem(nROBEntries, new ROBSignals())

  val canAllocate = robHead =/= (robTail + 2.U)

  val robTailIn1 = Wire(UInt(log2Ceil(nROBEntries).W))
  val robTailIn2 = Wire(UInt(log2Ceil(nROBEntries).W))

  robTailIn1 := robTail

  when(io.flush.valid) {
    printf(cf"ROB: flushing: ${io.flush.bits}\n")

    when(!entryValidList(io.flush.bits.robIdx)) {
      assert(robTail === io.flush.bits.robIdx, "Cannot flush invalid and non-tail entry")

      // allocate the rob
      robTailIn1 := robTail + 1.U

      entryValidList(io.flush.bits.robIdx) := true.B
      entryCommittedList(io.flush.bits.robIdx) := true.B
    }

    // NOTE: This operation will overwrite the program counter of the flushed instruction
    //       to the update program counter. And, the processor when looks at the retired
    //       instruction and find that it needs to be flushed, it will restart execution
    //       from the program counter of that instruction which will be the updated PC
    //       in this case.

    entryFlushedList(io.flush.bits.robIdx) := true.B

    when(io.flush.bits.updatePc.valid) {
      entryPCList(io.flush.bits.robIdx) := io.flush.bits.updatePc.bits
    }
  }

  when(io.exception.valid) {
    printf(cf"ROB: exception: ${io.exception.bits}\n")

    assert(entryValidList(io.exception.bits.robIdx), "Cannot except invalid entry")

    entryCommittedList(io.exception.bits.robIdx) := true.B
    entryExceptedList(io.exception.bits.robIdx) := true.B
  }

  robTailIn2 := robTailIn1

  when(io.allocate && canAllocate) {
    printf(cf"ROB: Allocating: head: $robHead, tail: $robTail\n")

    io.allocatedIdx.valid := true.B
    io.allocatedIdx.bits := robTail

    entryValidList(robTail) := false.B

    robTailIn2 := robTailIn1 + 1.U
  }.otherwise {
    printf(cf"ROB: Didn't allocate: head: $robHead, tail: $robTail, allocate: ${io.allocate}\n")

    io.allocatedIdx.valid := false.B
    io.allocatedIdx.bits := DontCare
  }

  robTail := robTailIn2

  when(io.instSignals.valid) {
    printf(cf"ROB: Storing instSignals: ${io.instSignals}\n")

    val robIdx = io.instSignals.bits.data.robIdx

    entryValidList(robIdx) := true.B
    entryCommittedList(robIdx) := false.B
    entryFlushedList(robIdx) := false.B
    entryExceptedList(robIdx) := false.B
    entryPCList(robIdx) := io.instSignals.bits.pc
    entryDataList(robIdx) := io.instSignals.bits.data
  }

  when(io.readRobIdx.valid) {
    assert(entryValidList(io.readRobIdx.bits), "input robIdx invalid")
    assert(!entryCommittedList(io.readRobIdx.bits), "Are you sure? you wanna read a committed instruction")

    io.robData.valid := true.B
    io.robData.bits.pc := entryPCList(io.readRobIdx.bits)
    io.robData.bits.data := entryDataList(io.readRobIdx.bits)
  }.otherwise {
    io.robData.valid := false.B
    io.robData.bits := DontCare
  }

  when(io.commitRobIdx0.valid) {
    printf(cf"ROB: committing ${io.commitRobIdx0} @ commitRobIdx0\n")

    assert(entryValidList(io.commitRobIdx0.bits), "Invalid instruction being committed")
    assert(!entryCommittedList(io.commitRobIdx0.bits), "Cannot commit already committed instruction")

    entryCommittedList(io.commitRobIdx0.bits) := true.B

    // NOTE: Head can be moved up by one position here itself
    // It would be advantageous too. A new instruction can be allocated
    // in the same cycle as this is being committed
    // Which is helpful in cases where the ROB is running full
    // Maybe this can be considered as an upgrade in the v2
  }

  when(io.commitRobIdx1.valid) {
    printf(cf"ROB: committing ${io.commitRobIdx1} @ commitRobIdx1\n")

    assert(entryValidList(io.commitRobIdx1.bits), "Invalid instruction being committed")
    assert(!entryCommittedList(io.commitRobIdx1.bits), "Cannot commit already committed instruction")

    entryCommittedList(io.commitRobIdx1.bits) := true.B
  }

  // keep commiting the instructions in the case of O3
  when(entryValidList(robHead) && entryCommittedList(robHead) && robHead =/= robTail) {
    printf(cf"ROB: retiring rob head: $robHead, tail: $robTail\n")

    val nextRobHead = robHead + 1.U

    entryValidList(robHead) := false.B

    io.retireInst.valid := true.B
    io.retireInst.bits.pc := entryPCList(robHead)
    io.retireInst.bits.flush := entryFlushedList(robHead)
    io.retireInst.bits.signals := entryDataList(robHead)

    // todo: currently not handling exceptions
    assert(!entryExceptedList(robHead), cf"Exception thrown at robIdx: $robHead")

    when(entryFlushedList(robHead)) {
      printf(cf"ROB: flushing\n")
      robTail := nextRobHead
    }

    robHead := nextRobHead
  }.otherwise {
    io.retireInst.valid := false.B
    io.retireInst.bits := DontCare
  }

  when(io.predictionRobIdx.valid) {
    val nextRobIdx = io.predictionRobIdx.bits + 1.U

    io.prediction.valid := entryValidList(nextRobIdx)
    io.prediction.bits.robIdx := nextRobIdx
    io.prediction.bits.pc := entryPCList(nextRobIdx)
  }.otherwise {
    io.prediction.valid := false.B
    io.prediction.bits := DontCare
  }

  io.robHead := robHead
}
