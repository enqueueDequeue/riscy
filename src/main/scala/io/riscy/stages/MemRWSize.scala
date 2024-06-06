package io.riscy.stages

import chisel3.{ChiselEnum, fromIntToLiteral}

object MemRWSize extends ChiselEnum {
  val BYTES_NO = Value(0x00.U)
  val BYTES_1U = Value(0x02.U)
  val BYTES_1S = Value(0x03.U)
  val BYTES_2U = Value(0x04.U)
  val BYTES_2S = Value(0x05.U)
  val BYTES_4U = Value(0x08.U)
  val BYTES_4S = Value(0x09.U)
  val BYTES_8U = Value(0x10.U)
  val BYTES_8S = Value(0x11.U)
}
