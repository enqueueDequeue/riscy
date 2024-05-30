package io.riscy

import chisel3.{Bool, Bundle, DontCare, Input, Mem, Module, Output, UInt, fromIntToWidth, when}

class Memory(addressWidth: Int, dataWidth: Int) extends Module {
  val MEMORY_SIZE = 1024

  val io = IO(new Bundle {
    val address = Input(UInt(addressWidth.W))
    val writeEn = Input(Bool())
    val writeData = Input(UInt(dataWidth.W))
    val readEn = Input(Bool())
    val readData = Output(UInt(dataWidth.W))
  })

  val memory = Mem(MEMORY_SIZE, UInt(dataWidth.W))

  when(io.readEn) {
    io.readData := memory(io.address)
  }.otherwise {
    io.readData := DontCare
  }

  when(io.writeEn) {
    memory(io.address) := io.writeData
  }
}
