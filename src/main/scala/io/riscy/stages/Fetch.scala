package io.riscy.stages

import chisel3.util.{Decoupled, Valid}
import chisel3.{Bundle, DontCare, Flipped, Input, Module, PrintableHelper, UInt, assert, fromBooleanToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.signals.Parameters

class Fetch()(implicit val params: Parameters) extends Module {
  val addrWidth = params.addrWidth
  val instWidth = params.instWidth

  val io = IO(new Bundle {
    val pc = Input(UInt(addrWidth.W))
    val inst = Valid(UInt(instWidth.W))
    val iReadAddr = Decoupled(UInt(addrWidth.W))
    val iReadValue = Flipped(Decoupled(UInt(instWidth.W)))
  })

  io.inst.valid := false.B
  io.inst.bits := DontCare

  printf(cf"fetching pc: ${io.pc}\n")

  io.iReadAddr.enq(io.pc)

  when(io.iReadAddr.fire) {
    when(io.iReadValue.valid) {
      val inst = io.iReadValue.deq()

      printf(cf"fetched: 0x$inst%x\n")

      io.inst.valid := true.B
      io.inst.bits := inst
    }.otherwise {
      io.iReadValue.nodeq()
    }
  }.otherwise {
    io.iReadValue.nodeq()
  }
}
