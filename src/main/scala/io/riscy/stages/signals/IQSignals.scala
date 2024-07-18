package io.riscy.stages.signals

import chisel3.{Bundle, UInt, fromIntToWidth}
import chisel3.util.{Valid, log2Ceil}

class IQSignals()(implicit params: Parameters) extends Bundle {
  val nPhyRegs = params.nPhyRegs
  val nEntries = params.nIQEntries
  val nROBEntries = params.nROBEntries

  val robIdx = UInt(log2Ceil(nROBEntries).W)
  val rs1PhyReg = Valid(UInt(log2Ceil(nPhyRegs).W))
  val rs2PhyReg = Valid(UInt(log2Ceil(nPhyRegs).W))
}

object IQSignals {
  def apply()(implicit params: Parameters): IQSignals = {
    new IQSignals()
  }
}
