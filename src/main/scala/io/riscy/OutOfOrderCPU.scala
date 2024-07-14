package io.riscy

import chisel3.util.{Decoupled, Valid}
import chisel3.{Bundle, Flipped, Module, UInt, fromIntToLiteral, fromIntToWidth}
import io.riscy.stages.signals.Defaults.{ADDR_WIDTH, BIT_WIDTH, DATA_WIDTH, INST_WIDTH, N_ARCH_REGISTERS, N_IQ_ENTRIES, N_PHY_REGISTERS}
import io.riscy.stages.{Decode, Execute, Fetch, InstructionQueue, PhyRegs, Rename}

class OutOfOrderCPU extends Module {

  val pc = 0.U(ADDR_WIDTH.W)

  val io = IO(new Bundle {
    val iReadAddr = Decoupled(UInt(ADDR_WIDTH.W))
    val iReadValue = Flipped(Decoupled(UInt(INST_WIDTH.W)))

    val dMem = Decoupled(new Bundle {
      val read = Valid(new Bundle {
        val len = UInt((DATA_WIDTH / BIT_WIDTH).W)
        val addr = UInt(ADDR_WIDTH.W)
      })

      val write = Valid(new Bundle {
        val len = UInt((DATA_WIDTH / BIT_WIDTH).W)
        val addr = UInt(ADDR_WIDTH.W)
        val value = UInt(DATA_WIDTH.W)
      })
    })

    val dMemRead = Flipped(Decoupled(new Bundle {
      val value = UInt(DATA_WIDTH.W)
    }))
  })

  // registers
  val registers = Module(new PhyRegs(N_PHY_REGISTERS))

  // Fetch
  val fetch = Module(new Fetch(ADDR_WIDTH, INST_WIDTH))

  // Decode
  val decode = Module(new Decode(INST_WIDTH, DATA_WIDTH))

  // Rename
  val rename = Module(new Rename(N_ARCH_REGISTERS, N_PHY_REGISTERS))

  // Issue
  // todo: make 4 a value in Defaults
  val instructionQueue = Module(new InstructionQueue(N_IQ_ENTRIES, N_PHY_REGISTERS, 4))

  // Reg Read
  // Dispatch

  // Execute
  val execute = Module(new Execute(DATA_WIDTH))

  // Commit
}
