package io.riscy.stages

import chisel3.{Bool, Bundle, Input, Module, Mux, Output, UInt, fromIntToWidth}
import io.riscy.stages.signals.Parameters

class WriteBack()(implicit val params: Parameters) extends Module {
  val dataWidth = params.dataWidth

  val io = IO(new Bundle {
    val memToReg = Input(Bool())
    val readData = Input(UInt(dataWidth.W))
    val execResult = Input(UInt(dataWidth.W))
    val result = Output(UInt(dataWidth.W))
  })

  io.result := Mux(io.memToReg, io.readData, io.execResult)
}
