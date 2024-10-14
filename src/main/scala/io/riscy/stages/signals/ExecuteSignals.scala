package io.riscy.stages.signals

import chisel3.{Bool, Bundle, Output, UInt, fromIntToWidth}

class ExecuteSignals(addressWidth:Int, dataWidth: Int) extends Bundle {
  val nextPc = Output(UInt(addressWidth.W))
  val result = Output(UInt(dataWidth.W))
  val zero = Output(Bool())
}

object ExecuteSignals {
  def apply()(implicit params: Parameters): ExecuteSignals = new ExecuteSignals(params.addrWidth, params.dataWidth)
}
