package io.riscy

import chisel3.ChiselEnum

object ExecuteOp extends ChiselEnum {
  val NOP = Value
  val ADD = Value
  val SUB = Value
  val XOR = Value
  val OR = Value
  val AND = Value

  // shift left logical
  val SLL = Value
  // shift right logical
  val SRL = Value
  // shift right arithmetic
  val SRA = Value

  // set less than
  val SLT = Value
  // set less than unsigned
  val SLTU = Value

  val FW_A = Value
  val FW_B = Value
}
