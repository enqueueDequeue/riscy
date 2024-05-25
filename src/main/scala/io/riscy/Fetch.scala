package io.riscy

import chisel3.util.Decoupled
import chisel3.{Bundle, Input, Module, UInt, fromIntToWidth, when}

class Fetch(val addressWidth: Int, iCacheGen: () => Cache) extends Module {
  val DATA_WIDTH = 32

  val io = IO(new Bundle {
    val pc = Input(UInt(addressWidth.W))
    val inst = Decoupled(UInt(DATA_WIDTH.W))
  })

  val iCache = Module(iCacheGen())

  assert(DATA_WIDTH == iCache.dataWidth)
  assert(addressWidth == iCache.addressWidth)

  when(iCache.io.read.ready) {
    iCache.io.read.enq(io.pc)
  }.otherwise {
    iCache.io.read.noenq()
  }

  iCache.io.write.noenq()

  iCache.io.readValue <> io.inst
}
