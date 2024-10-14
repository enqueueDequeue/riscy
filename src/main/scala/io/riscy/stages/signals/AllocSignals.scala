package io.riscy.stages.signals

import chisel3.{Bundle, UInt, fromIntToWidth}
import chisel3.util.{Valid, log2Ceil}
import io.riscy.stages.LoadStoreIndex

class AllocSignals()(implicit params: Parameters) extends Bundle {
  val dstReg = Valid(UInt(log2Ceil(params.nPhyRegs).W))
  val robIdx = UInt(log2Ceil(params.nROBEntries).W)
  val iqIdx = UInt(log2Ceil(params.nIQEntries).W)
  val memIdx = Valid(new LoadStoreIndex())
}

object AllocSignals {
  def apply()(implicit params: Parameters): AllocSignals = new AllocSignals()
}
