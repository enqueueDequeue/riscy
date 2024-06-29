package io.riscy.stages

import chisel3.util.Decoupled
import chisel3.{Bundle, Flipped, Input, Module, UInt, fromIntToWidth, when}

class Fetch(addressWidth: Int, instructionWidth: Int) extends Module {

  val io = IO(new Bundle {
    val pc = Input(UInt(addressWidth.W))
    val inst = Decoupled(UInt(instructionWidth.W))
    val iReadAddr = Decoupled(UInt(addressWidth.W))
    val iReadValue = Flipped(Decoupled(UInt(instructionWidth.W)))
  })

  when(io.iReadAddr.ready) {
    io.iReadAddr.enq(io.pc)
  }.otherwise {
    io.iReadAddr.noenq()
  }

  io.iReadValue <> io.inst
}
