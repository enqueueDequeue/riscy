package io.riscy

import chisel3.util.Decoupled
import chisel3.{Bundle, Input, Module, UInt, fromIntToWidth, when}

class Fetch(addressWidth: Int, instructionWidth: Int, iCacheGen: () => Cache) extends Module {

  val io = IO(new Bundle {
    val pc = Input(UInt(addressWidth.W))
    val inst = Decoupled(UInt(instructionWidth.W))
  })

  val iCache = Module(iCacheGen())

  assert(instructionWidth == iCache.dataWidth)
  assert(addressWidth == iCache.addressWidth)

  when(iCache.io.read.ready) {
    iCache.io.read.enq(io.pc)
  }.otherwise {
    iCache.io.read.noenq()
  }

  iCache.io.write.noenq()

  iCache.io.readValue <> io.inst
}
