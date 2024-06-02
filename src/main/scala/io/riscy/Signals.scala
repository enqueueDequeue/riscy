package io.riscy

import chisel3.util.log2Ceil
import chisel3.{Bool, Bundle, UInt, fromIntToWidth}

class Signals(nArchRegisters:Int, dataWidth: Int) extends Bundle {
  val branch = Bool()
  val memRead = Bool()
  val memToReg = Bool()
  val memWrite = Bool()
  val aluSrc = Bool()
  val regWrite = Bool()
  val bNot = Bool()
  val aluOp = ExecuteOp()
  val immediate = UInt(dataWidth.W)
  val rs1 = UInt(log2Ceil(nArchRegisters).W)
  val rs2 = UInt(log2Ceil(nArchRegisters).W)
  val rd = UInt(log2Ceil(nArchRegisters).W)
}
