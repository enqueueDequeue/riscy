package io.riscy

import chisel3.util.{Cat, Fill}
import chisel3.{Bundle, Input, Module, Output, UInt, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, when}
import io.riscy.Decode.{INST_WIDTH, N_ARCH_REGISTERS, signExtend}

class Decode(instructionWidth: Int, dataWidth: Int) extends Module {
  assert(instructionWidth == INST_WIDTH)

  val io = IO(new Bundle {
    val inst = Input(UInt(instructionWidth.W))
    val signals = Output(new Signals(N_ARCH_REGISTERS, dataWidth))
  })

  io.signals.branch := false.B
  io.signals.memRead := false.B
  io.signals.memToReg := false.B
  io.signals.memWrite := false.B
  io.signals.aluSrc := false.B
  io.signals.regWrite := false.B
  io.signals.bNot := true.B
  io.signals.immediate := 0.U
  io.signals.rs1 := io.inst(19, 15)
  io.signals.rs2 := io.inst(24, 20)
  io.signals.rd := io.inst(11, 7)

  when(OpCode.ADD === io.inst) {
    io.signals.aluOp := ExecuteOp.ADD
  }.elsewhen(OpCode.ADDI === io.inst) {
    io.signals.aluOp := ExecuteOp.ADD
  }.elsewhen(OpCode.AND === io.inst) {
    io.signals.aluOp := ExecuteOp.AND
  }.elsewhen(OpCode.ANDI === io.inst) {
    io.signals.aluOp := ExecuteOp.AND
  }.elsewhen(OpCode.AUIPC === io.inst) {
    io.signals.aluOp := ExecuteOp.ADD
  }.elsewhen(OpCode.BEQ === io.inst) {
    io.signals.aluOp := ExecuteOp.SUB
  }.elsewhen(OpCode.BGE === io.inst) {
    io.signals.aluOp := ExecuteOp.SLT
  }.elsewhen(OpCode.BGEU === io.inst) {
    io.signals.aluOp := ExecuteOp.SLT
  }.elsewhen(OpCode.BLT === io.inst) {
    io.signals.aluOp := ExecuteOp.SLT
  }.elsewhen(OpCode.BLTU === io.inst) {
    io.signals.aluOp := ExecuteOp.SLT
  }.elsewhen(OpCode.BNE === io.inst) {
    io.signals.aluOp := ExecuteOp.SUB
  }.elsewhen(OpCode.EBREAK === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.ECALL === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.FENCE === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.FENCE_TSO === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.JAL === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.JALR === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.LB === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.LBU === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.LH === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.LHU === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.LUI === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.LW === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.OR === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.ORI === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.PAUSE === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.SB === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.SBREAK === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.SCALL === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.SH === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.SLL === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.SLT === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.SLTI === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.SLTIU === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.SLTU === io.inst) {
    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.SRA === io.inst) {
    io.signals.aluOp := ExecuteOp.SRA
  }.elsewhen(OpCode.SRL === io.inst) {
    io.signals.aluOp := ExecuteOp.SRL
  }.elsewhen(OpCode.SUB === io.inst) {
    io.signals.aluOp := ExecuteOp.SUB
  }.elsewhen(OpCode.SW === io.inst) {
    io.signals.aluOp := ExecuteOp.ADD
  }.elsewhen(OpCode.XOR === io.inst) {
    io.signals.aluOp := ExecuteOp.XOR
  }.elsewhen(OpCode.XORI === io.inst) {
    io.signals.aluOp := ExecuteOp.XOR
  }.otherwise {
    io.signals.aluOp := ExecuteOp.NOP
  }
}

object Decode {
  val INST_WIDTH = 32
  val N_ARCH_REGISTERS = 32

  def signExtend(value: UInt, sourceWidth: Int, targetWidth: Int): UInt = {
    Cat(Fill(targetWidth, value(sourceWidth - 1)))(targetWidth - 1, 0)
  }
}
