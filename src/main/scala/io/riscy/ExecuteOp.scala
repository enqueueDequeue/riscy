package io.riscy

import chisel3.ChiselEnum

object ExecuteOp extends ChiselEnum {
  val NOP = Value
  val ADD = Value
  val SUB = Value
  val SLL = Value
  val SRL = Value
  val SRA = Value
  val SLT = Value
  val SLU = Value
  val XOR = Value
  val OR = Value
  val AND = Value
}
