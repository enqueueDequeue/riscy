package io.riscy

import chisel3.util.{Decoupled, Valid}
import chisel3.{Bundle, Flipped, Module, UInt, fromIntToLiteral, fromIntToWidth}
import io.riscy.stages.signals.Parameters
import io.riscy.stages.{Decode, Execute, Fetch, InstructionQueue, PhyRegs, Rename}

class OutOfOrderCPU()(implicit val params: Parameters) extends Module {

  val pc = 0.U(params.addrWidth.W)

  val io = IO(new Bundle {
    val iReadAddr = Decoupled(UInt(params.addrWidth.W))
    val iReadValue = Flipped(Decoupled(UInt(params.instWidth.W)))

    val dMem = Decoupled(new Bundle {
      val read = Valid(new Bundle {
        val len = UInt((params.dataWidth / params.bitWidth).W)
        val addr = UInt(params.addrWidth.W)
      })

      val write = Valid(new Bundle {
        val len = UInt((params.dataWidth / params.bitWidth).W)
        val addr = UInt(params.addrWidth.W)
        val value = UInt(params.dataWidth.W)
      })
    })

    val dMemRead = Flipped(Decoupled(new Bundle {
      val value = UInt(params.dataWidth.W)
    }))
  })

  // registers
  val registers = Module(new PhyRegs())

  // Fetch
  val fetch = Module(new Fetch())

  // Decode
  val decode = Module(new Decode())

  // ROB allocate

  // Rename
  val rename = Module(new Rename())

  // Issue
  val instructionQueue = Module(new InstructionQueue())

  // Reg Read
  // Dispatch

  // Execute
  val execute = Module(new Execute())

  // Commit
}
