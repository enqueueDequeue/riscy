package io.riscy

import chisel3.util.log2Ceil
import chisel3.{Bool, Bundle, Input, Mem, Module, Mux, Output, UInt, fromIntToLiteral, fromIntToWidth, when}

class PhyRegs(nRegisters: Int) extends Module {
  private val DATA_WIDTH = 64

  val io = IO(new Bundle {
    val rs1 = Input(UInt(log2Ceil(nRegisters).W))
    val rs2 = Input(UInt(log2Ceil(nRegisters).W))
    val rdEn = Input(Bool())
    val rd = Input(UInt(log2Ceil(nRegisters).W))
    val rs1Value = Output(UInt(DATA_WIDTH.W))
    val rs2Value = Output(UInt(DATA_WIDTH.W))
    val rdValue = Input(UInt(DATA_WIDTH.W))
  })

  val regs = Mem(nRegisters, UInt(DATA_WIDTH.W))

  io.rs1Value := Mux(io.rs1 === 0.U, 0.U, regs(io.rs1))
  io.rs2Value := Mux(io.rs2 === 0.U, 0.U, regs(io.rs2))

  when(io.rdEn) {
    regs(io.rd) := io.rdValue
  }
}
