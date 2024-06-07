package io.riscy.stages.signals

import chisel3.{Bundle, UInt, fromIntToWidth}

class RegReadSignals(dataWidth: Int) extends Bundle {
  val rs1Value = UInt(dataWidth.W)
  val rs2Value = UInt(dataWidth.W)
}

object RegReadSignals {
  def apply(): RegReadSignals = new RegReadSignals(Defaults.DATA_WIDTH)
}
