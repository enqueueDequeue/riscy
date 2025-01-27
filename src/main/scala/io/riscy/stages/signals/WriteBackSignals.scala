package io.riscy.stages.signals

import chisel3.{Bundle, Output, UInt, fromIntToWidth}

class WriteBackSignals(dataWidth: Int) extends Bundle {
  val result = Output(UInt(dataWidth.W))
}

object WriteBackSignals {
  def apply()(implicit params: Parameters): WriteBackSignals = new WriteBackSignals(params.dataWidth)
}
