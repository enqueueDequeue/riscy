package io.riscy

import chisel3.{Bundle, Input, Module, UInt, fromIntToWidth}

class Decode extends Module {
  val INST_WIDTH = 32

  val io = IO(new Bundle {
    val inst = Input(UInt(INST_WIDTH.W))
  })

  val phyRegs = Module(new PhyRegs(32))

  // todo: identify the instruction
  //  generate the signals
  //  AND, read the registers
}
