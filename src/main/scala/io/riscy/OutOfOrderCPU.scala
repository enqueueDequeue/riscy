package io.riscy

import chisel3.util.{Decoupled, Valid}
import chisel3.{Bundle, Flipped, Module, RegInit, UInt, fromIntToLiteral, fromIntToWidth}
import io.riscy.stages.signals.Parameters
import io.riscy.stages.{Decode, Execute, Fetch, InstructionQueue, PhyRegs, ROB, Rename}

class OutOfOrderCPU()(implicit val params: Parameters) extends Module {
  val addrWidth = params.addrWidth
  val nROBEntries = params.nROBEntries

  val pc = RegInit(0.U(addrWidth.W))

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

  // Fetch
  val fetch = Module(new Fetch())

  // Decode
  val decode = Module(new Decode())

  // Rename
  val rename = Module(new Rename())

  // ROB allocate
  val rob = Module(new ROB())

  // Issue
  val instructionQueue = Module(new InstructionQueue())

  // Reg Read
  val registers = Module(new PhyRegs())

  // Dispatch

  // Execute
  val execute = Module(new Execute())

  // Commit
}
