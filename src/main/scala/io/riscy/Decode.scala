package io.riscy

import chisel3.{Bundle, Input, Module, Output, UInt, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth}

class Decode(instructionWidth: Int, dataWidth: Int) extends Module {
  val INST_WIDTH = 32

  assert(instructionWidth == INST_WIDTH)

  val io = IO(new Bundle {
    val inst = Input(UInt(instructionWidth.W))
    val signals = Output(new Signals(dataWidth))
  })

  io.signals.branch := true.B
  io.signals.memRead := true.B
  io.signals.memToReg := true.B
  io.signals.memWrite := true.B
  io.signals.aluSrc := true.B
  io.signals.regWrite := true.B
  io.signals.aluOp := OpCode.XOR
  io.signals.bNot := true.B
  io.signals.immediate := 0.U
}
