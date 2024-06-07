package io.riscy.stages.signals

import chisel3.{Bundle, Output, UInt, fromIntToWidth}

class MemorySignals(dataWidth: Int) extends Bundle {
  val readData = Output(UInt(dataWidth.W))
}

object MemorySignals {
  def apply(): MemorySignals = new MemorySignals(Defaults.DATA_WIDTH)
}
