package io.riscy

import chisel3.util.log2Ceil
import chisel3.{Bool, Bundle, DontCare, Input, Mem, Module, Mux, Output, UInt, fromIntToLiteral, fromIntToWidth, when}

class PhyRegs(nRegisters: Int) extends Module {
  private val DATA_WIDTH = 64

  val io = IO(new Bundle {
    val rs1En = Input(Bool())
    val rs1 = Input(UInt(log2Ceil(nRegisters).W))
    val rs2En = Input(Bool())
    val rs2 = Input(UInt(log2Ceil(nRegisters).W))
    val rdEn = Input(Bool())
    val rd = Input(UInt(log2Ceil(nRegisters).W))
    val rs1Value = Output(UInt(DATA_WIDTH.W))
    val rs2Value = Output(UInt(DATA_WIDTH.W))
    val rdValue = Input(UInt(DATA_WIDTH.W))
  })

  val values = Mem(nRegisters, UInt(DATA_WIDTH.W))

  when(io.rs1En) {
    io.rs1Value := Mux(io.rs1 === 0.U, 0.U, values(io.rs1))
  }.otherwise {
    io.rs1Value := DontCare
  }

  when(io.rs2En) {
    io.rs2Value := Mux(io.rs2 === 0.U, 0.U, values(io.rs2))
  }.otherwise {
    io.rs2Value := DontCare
  }

  when(io.rdEn) {
    values(io.rd) := io.rdValue
  }.otherwise {
    io.rdValue := DontCare
  }
}
