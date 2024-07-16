package io.riscy.stages

import chisel3.util.Decoupled
import chisel3.{Bundle, Flipped, Input, Module, UInt, fromIntToWidth, when}
import io.riscy.stages.signals.Parameters

class Fetch()(implicit val params: Parameters) extends Module {
  val addrWidth = params.addrWidth
  val instWidth = params.instWidth

  val io = IO(new Bundle {
    val pc = Input(UInt(addrWidth.W))
    val inst = Decoupled(UInt(instWidth.W))
    val iReadAddr = Decoupled(UInt(addrWidth.W))
    val iReadValue = Flipped(Decoupled(UInt(instWidth.W)))
  })

  when(io.iReadAddr.ready) {
    io.iReadAddr.enq(io.pc)
  }.otherwise {
    io.iReadAddr.noenq()
  }

  io.iReadValue <> io.inst
}
