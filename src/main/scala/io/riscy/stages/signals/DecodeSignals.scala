package io.riscy.stages.signals

import chisel3.util.log2Ceil
import chisel3.{Bool, Bundle, UInt, fromIntToWidth}
import io.riscy.stages.signals.Defaults.{DATA_WIDTH, N_ARCH_REGISTERS}
import io.riscy.stages.{ExecuteOp, MemRWSize}

class DecodeSignals(nArchRegisters:Int, dataWidth: Int) extends Bundle {
  val jump = Bool()
  val branch = Bool()
  // Actually, memToReg is true whenever
  // memRead is not BYTES_NO
  // this logic can be simplified
  val word = Bool()
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

object DecodeSignals {
  def apply(): DecodeSignals = new DecodeSignals(N_ARCH_REGISTERS, DATA_WIDTH)
}
