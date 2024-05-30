package io.riscy

import chisel3.{Bool, Bundle, UInt, fromIntToWidth}

class Signals(dataWidth: Int) extends Bundle {
  val branch = Bool()
  val memRead = Bool()
  val memToReg = Bool()
  val memWrite = Bool()
  val aluSrc = Bool()
  val regWrite = Bool()
  val bNot = Bool()
  val aluOp = OpCode()
  val immediate = UInt(dataWidth.W)
}
