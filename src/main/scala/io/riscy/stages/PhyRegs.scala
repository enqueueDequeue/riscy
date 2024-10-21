package io.riscy.stages

import chisel3.util.log2Ceil
import chisel3.{Bool, Bundle, Input, Mem, Module, Mux, Output, UInt, fromIntToLiteral, fromIntToWidth, when}
import io.riscy.stages.signals.Parameters

class PhyRegs()(implicit val params: Parameters) extends Module {
  val nPhyRegs = params.nPhyRegs
  val dataWidth = params.dataWidth

  val io = IO(new Bundle {
    val rs1 = Input(UInt(log2Ceil(nPhyRegs).W))
    val rs2 = Input(UInt(log2Ceil(nPhyRegs).W))
    val rdEn = Input(Bool())
    val rd = Input(UInt(log2Ceil(nPhyRegs).W))
    val rs1Value = Output(UInt(dataWidth.W))
    val rs2Value = Output(UInt(dataWidth.W))
    val rdValue = Input(UInt(dataWidth.W))
    val rd2En = Input(Bool())
    val rd2 = Input(UInt(log2Ceil(nPhyRegs).W))
    val rd2Value = Input(UInt(dataWidth.W))
  })

  val regs = Mem(nPhyRegs, UInt(dataWidth.W))

  io.rs1Value := regs(io.rs1)
  io.rs2Value := regs(io.rs2)

  when(io.rdEn) {
    regs(io.rd) := io.rdValue
  }

  when(io.rd2En) {
    regs(io.rd2) := io.rd2Value
  }
}
