package io.riscy.stages.signals

import chisel3.util.log2Ceil
import chisel3.{Bundle, UInt, fromIntToWidth}

class IQSignals(nROBEntries: Int) extends Bundle {
  val robIdx = UInt(log2Ceil(nROBEntries).W)
}

object IQSignals {
  def apply()(implicit params: Parameters): IQSignals = {
    new IQSignals(params.nROBEntries)
  }
}
