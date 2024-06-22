package io.riscy.stages

import chisel3.util.{Cat, Fill, is, log2Ceil, switch}
import chisel3.{Bool, Bundle, Input, Module, Mux, Output, UInt, Wire, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth}
import io.riscy.stages.signals.Defaults.WORD_WIDTH

class Execute(dataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(dataWidth.W))
    val b = Input(UInt(dataWidth.W))
    val branchInvert = Input(Bool())
    val word = Input(Bool())
    val op = Input(ExecuteOp())
    val result = Output(UInt(dataWidth.W))
    val zero = Output(Bool())
  })

  val shamt = io.b(log2Ceil(dataWidth) - 1, 0)
  val zeroInternal = Wire(Bool())

  val maskW0 = Fill(WORD_WIDTH, 0.U(1.W))
  val maskW1 = Fill(WORD_WIDTH, 1.U(1.W))

  val mask0 = Cat(maskW0, maskW1)
  val mask1 = Cat(maskW1, maskW0)

  val aWord = Mux(io.a(WORD_WIDTH - 1), io.a | mask1, io.a & mask0)
  val bWord = Mux(io.b(WORD_WIDTH - 1), io.b | mask1, io.b & mask0)

  val a = Mux(!io.word, io.a, aWord)
  val b = Mux(!io.word, io.b, bWord)

  io.zero := Mux(io.branchInvert, !zeroInternal, zeroInternal)
  io.result := 0.U

  zeroInternal := false.B

  switch(io.op) {
    is(ExecuteOp.ADD) {
      io.result := a + b
    }
    is(ExecuteOp.SLL) {
      io.result := (a << shamt)(dataWidth - 1, 0)
    }
    is(ExecuteOp.SRL) {
      io.result := Cat(0.U(dataWidth.W), a >> shamt)(dataWidth - 1, 0)
    }
    is(ExecuteOp.SRA) {
      io.result := Cat(Fill(dataWidth, a(dataWidth - 1)), a >> shamt)(dataWidth - 1, 0)
    }
    is(ExecuteOp.XOR) {
      io.result := a ^ b
    }
    is(ExecuteOp.OR) {
      io.result := a | b
    }
    is(ExecuteOp.AND) {
      io.result := a & b
    }
    is(ExecuteOp.SUB) {
      val result = a - b

      zeroInternal := (result === 0.U)
      io.result := result
    }
    is(ExecuteOp.SLT) {
      val result = (a.asSInt < b.asSInt).asUInt

      zeroInternal := result.asBool
      io.result := Cat(0.U((dataWidth - 1).W), result)
    }
    is(ExecuteOp.SLTU) {
      val result = (a < b).asUInt

      zeroInternal := result.asBool
      io.result := Cat(0.U((dataWidth - 1).W), result)
    }
    is(ExecuteOp.FW_A) {
      io.result := a
    }
    is(ExecuteOp.FW_B) {
      io.result := b
    }
  }
}
