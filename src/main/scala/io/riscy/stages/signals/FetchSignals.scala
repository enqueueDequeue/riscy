package io.riscy.stages.signals

import chisel3.util.Valid
import chisel3.{Bundle, UInt, fromIntToWidth}

class FetchSignals(instructionWidth: Int) extends Bundle {
  val instruction = Valid(UInt(instructionWidth.W))
}

object FetchSignals {
  def apply()(implicit params: Parameters): FetchSignals = new FetchSignals(params.instWidth)
}
