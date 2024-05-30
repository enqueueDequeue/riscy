package io.riscy

import chisel3.{Mem, UInt, fromBooleanToLiteral, fromIntToWidth, when}

class RegCache(addressWidth: Int, dataWidth: Int) extends Cache(addressWidth, dataWidth) {
  val NUM_ENTRIES = 1024

  val values = Mem(NUM_ENTRIES, UInt(dataWidth.W))

  // always ready to read and write the stuff
  io.read.ready := true.B
  io.write.ready := true.B

  when(io.read.fire) {
    io.readValue.enq(values(io.read.bits))
  }.otherwise {
    io.readValue.noenq()
  }

  when(io.write.valid) {
    values(io.write.bits.address) := io.write.bits.content
  }
}
