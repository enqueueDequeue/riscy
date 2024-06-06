package io.riscy.stages

import chisel3.{Bool, Bundle, Input, Module, Mux, Output, UInt, fromIntToWidth}

class WriteBack(dataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val memToReg = Input(Bool())
    val readData = Input(UInt(dataWidth.W))
    val execResult = Input(UInt(dataWidth.W))
    val result = Output(UInt(dataWidth.W))
  })

  io.result := Mux(io.memToReg, io.readData, io.execResult)
}
