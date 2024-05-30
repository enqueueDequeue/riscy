package io.riscy

import chisel3.util.{Cat, Fill, is, log2Ceil, switch}
import chisel3.{Bool, Bundle, Input, Module, Output, UInt, fromIntToLiteral, fromIntToWidth, when}

class Execute(dataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(dataWidth.W))
    val b = Input(UInt(dataWidth.W))
    val bNot = Input(Bool())
    val op = Input(OpCode())
    val result = Output(UInt(dataWidth.W))
    val zero = Output(Bool())
  })

  val shamt = io.b(log2Ceil(dataWidth) - 1, 0)

  when(io.bNot) {
    io.zero := io.a =/= io.b
  }.otherwise {
    io.zero := io.a === io.b
  }

  io.result := 0.U

  switch(io.op) {
    is(OpCode.ADD) {
      io.result := io.a + io.b
    }
    is(OpCode.SUB) {
      io.result := io.a - io.b
    }
    is(OpCode.SLL) {
      io.result := (io.a << shamt)(dataWidth - 1, 0)
    }
    is(OpCode.SRL) {
      io.result := Cat(0.U(dataWidth.W), io.a >> shamt)(dataWidth - 1, 0)
    }
    is(OpCode.SRA) {
      io.result := Cat(Fill(dataWidth, io.a(dataWidth - 1)), io.a >> shamt)(dataWidth - 1, 0)
    }
    is(OpCode.SLT) {
      io.result := Cat(0.U((dataWidth - 1).W), (io.a.asSInt < io.b.asSInt).asUInt)
    }
    is(OpCode.SLU) {
      io.result := Cat(0.U((dataWidth - 1).W), (io.a < io.b).asUInt)
    }
    is(OpCode.XOR) {
      io.result := io.a ^ io.b
    }
    is(OpCode.OR) {
      io.result := io.a | io.b
    }
    is(OpCode.AND) {
      io.result := io.a & io.b
    }
  }
}
