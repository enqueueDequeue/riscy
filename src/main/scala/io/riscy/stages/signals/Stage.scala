package io.riscy.stages.signals

import chisel3.{Bundle, Data, UInt, fromIntToWidth}
import io.riscy.stages.signals.Defaults.INST_WIDTH

class Stage[+T <: Data](instructionWidth: Int, gen: T) extends Bundle {
  val pc = UInt(instructionWidth.W)
  val stage = gen
}

object Stage {
  def apply[T <: Data](gen: T): Stage[T] = new Stage(INST_WIDTH, gen)
}
