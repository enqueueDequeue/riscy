package io.riscy

import chisel3.{Module, fromIntToLiteral, fromIntToWidth}
import io.riscy.stages.{Decode, Execute, Fetch, PhyRegs, Rename}
import io.riscy.stages.signals.Defaults.{ADDR_WIDTH, DATA_WIDTH, INST_WIDTH, N_ARCH_REGISTERS, N_PHY_REGISTERS}

class OutOfOrderCPU extends Module {

  val pc = 0.U(ADDR_WIDTH.W)

  // registers
  val registers = new PhyRegs(N_PHY_REGISTERS)

  // Fetch
  val fetch = new Fetch(ADDR_WIDTH, INST_WIDTH)

  // Decode
  val decode = new Decode(INST_WIDTH, DATA_WIDTH)

  // Rename
  val rename = new Rename(N_ARCH_REGISTERS, N_PHY_REGISTERS)

  // Issue
  // Reg Read
  // Dispatch

  // Execute
  val execute = new Execute(DATA_WIDTH)

  // Commit
}
