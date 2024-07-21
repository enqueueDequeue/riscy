package io.riscy.stages

import chisel3.util.{Valid, isPow2, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Input, Mem, Module, Output, PrintableHelper, RegInit, UInt, assert, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.signals.{Parameters, ROBSignals}

class ROB()(implicit val params: Parameters) extends Module {
  val nROBEntries = params.nROBEntries

  require(isPow2(nROBEntries))

  val io = IO(new Bundle {
    // instruction allocation
    val instSignals = Input(Valid(ROBSignals()))
    val robIdx = Output(Valid(UInt(log2Ceil(nROBEntries).W)))

    // instruction dispatch
    val readRobIdx = Input(Valid(UInt(log2Ceil(nROBEntries).W)))
    val robData = Output(Valid(ROBSignals()))

    // instruction commit
    val commitRobIdx = Input(Valid(UInt(log2Ceil(nROBEntries).W)))

    // instruction retire
    val retireInst = Output(Valid(ROBSignals()))
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
    val data = ROBSignals()
  }))

  val canAllocate = robHead =/= (robTail + 1.U)

  when(io.instSignals.valid && canAllocate) {
    printf(cf"Allocating: head: $robHead, tail: $robTail, instSignals: ${io.instSignals}\n")

    entries(robTail).valid := true.B
    entries(robTail).bits.committed := false.B
    entries(robTail).bits.data := io.instSignals.bits

    io.robIdx.valid := true.B
    io.robIdx.bits := robTail

    robTail := robTail + 1.U
  }.otherwise {
    printf(cf"Didn't allocate: head: $robHead, tail: $robTail, instSignals.valid: ${io.instSignals.valid}\n")

    io.robIdx.valid := false.B
    io.robIdx.bits := DontCare
  }

  when(io.readRobIdx.valid) {
    assert(entries(io.robIdx.bits).valid, "input robIdx invalid")
    assert(!entries(io.robIdx.bits).bits.committed, "Are you sure? you wanna read a committed instruction")

    io.robData.valid := true.B
    io.robData.bits := entries(io.robIdx.bits).bits.data
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
    entries(robHead).valid := false.B

    io.retireInst.valid := true.B
    io.retireInst.bits := entries(robHead).bits.data

    robHead := robHead + 1.U
  }.otherwise {
    io.retireInst.valid := false.B
    io.retireInst.bits := DontCare
  }
}
