package io.riscy.stages.signals

import chisel3.{Bundle, Data, UInt, fromIntToWidth}

class Stage[+T <: Data](instructionWidth: Int, gen: T) extends Bundle {
  val pc = UInt(instructionWidth.W)
  val stage = gen
}

object Stage {
  def apply[T <: Data](gen: T)(implicit params: Parameters): Stage[T] = new Stage(params.instWidth, gen)
}
