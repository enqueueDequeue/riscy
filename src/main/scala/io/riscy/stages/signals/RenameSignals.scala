package io.riscy.stages.signals

import chisel3.util.{Valid, log2Ceil}
import chisel3.{Bundle, UInt, fromIntToWidth}

class RenameSignals(nPhyRegs: Int) extends Bundle {
  val rs1PhyReg = UInt(log2Ceil(nPhyRegs).W)
  val rs2PhyReg = UInt(log2Ceil(nPhyRegs).W)
  val rdPhyReg = Valid(UInt(log2Ceil(nPhyRegs).W))
}

object RenameSignals {
  def apply()(implicit params: Parameters): RenameSignals = new RenameSignals(params.nPhyRegs)
}
