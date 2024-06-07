package io.riscy.stages.signals

import chisel3.util.Valid
import chisel3.{Bundle, UInt, fromIntToWidth}
import io.riscy.stages.signals.Defaults.INST_WIDTH

class FetchSignals(instructionWidth: Int) extends Bundle {
  val instruction = Valid(UInt(instructionWidth.W))
}

object FetchSignals {
  def apply(): FetchSignals = new FetchSignals(INST_WIDTH)
}
