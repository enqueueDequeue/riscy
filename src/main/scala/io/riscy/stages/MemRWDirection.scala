package io.riscy.stages

import chisel3.ChiselEnum

object MemRWDirection extends ChiselEnum {
  val read = Value
  val write = Value
}
