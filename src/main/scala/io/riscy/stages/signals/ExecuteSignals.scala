package io.riscy.stages.signals

import chisel3.{Bool, Bundle, Output, UInt, fromIntToWidth}

class ExecuteSignals(dataWidth: Int) extends Bundle {
  val result = Output(UInt(dataWidth.W))
  val zero = Output(Bool())
}

object ExecuteSignals {
  def apply(): ExecuteSignals = new ExecuteSignals(Defaults.DATA_WIDTH)
}
