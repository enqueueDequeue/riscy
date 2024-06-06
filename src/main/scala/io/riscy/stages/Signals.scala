package io.riscy.stages

import chisel3.util.log2Ceil
import chisel3.{Bool, Bundle, UInt, fromIntToWidth}
import io.riscy.stages.MemRWSize

class Signals(nArchRegisters:Int, dataWidth: Int) extends Bundle {
  val jump = Bool()
  val branch = Bool()
  val memToReg = Bool()
  val memRead = MemRWSize()
  val memWrite = MemRWSize()
  val rs1Pc = Bool()
  val rs2Imm = Bool()
  val regWrite = Bool()
  val branchInvert = Bool()
  val aluOp = ExecuteOp()
  val immediate = UInt(dataWidth.W)
  val rs1 = UInt(log2Ceil(nArchRegisters).W)
  val rs2 = UInt(log2Ceil(nArchRegisters).W)
  val rd = UInt(log2Ceil(nArchRegisters).W)
}
