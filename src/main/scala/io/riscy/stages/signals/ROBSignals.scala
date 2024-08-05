package io.riscy.stages.signals

import chisel3.{Bundle, UInt, fromIntToWidth}
import chisel3.util.{Valid, log2Ceil}

class ROBSignals()(implicit p: Parameters) extends Bundle {
  val robIdx = Valid(UInt(log2Ceil(p.nROBEntries).W))
}

object ROBSignals {
  def apply()(implicit p: Parameters): ROBSignals = new ROBSignals()
}
