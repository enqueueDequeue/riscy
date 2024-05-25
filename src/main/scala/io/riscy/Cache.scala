package io.riscy

import chisel3.util.Decoupled
import chisel3.{Bundle, Flipped, Module, UInt, fromIntToWidth}

abstract class Cache(val addressWidth: Int, val dataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val read = Flipped(Decoupled(UInt(addressWidth.W)))
    val readValue = Decoupled(UInt(dataWidth.W))

    val write = Flipped(Decoupled(new Bundle {
      val address = UInt(addressWidth.W)
      val content = UInt(dataWidth.W)
    }))
  })
}
