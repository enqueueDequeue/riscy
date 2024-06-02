package io.riscy

import chisel3.util.Decoupled
import chisel3.{Bundle, Flipped, Module, Mux, RegInit, UInt, Wire, fromIntToLiteral, fromIntToWidth, when}

class InOrderCPU extends Module {
  // The magic instruction
  // Decode to find why this is the NOP
  val NOP = 0x33

  val N_ARCH_REGISTERS = 32
  val ARCH_WIDTH = 64
  val ADDR_WIDTH = ARCH_WIDTH
  val DATA_WIDTH = ARCH_WIDTH
  val INST_WIDTH = 32

  val io = IO(new Bundle {
    val iReadAddr = Decoupled(UInt(ADDR_WIDTH.W))
    val iReadValue = Flipped(Decoupled(UInt(INST_WIDTH.W)))
  })

  val pc = RegInit(0.U(ARCH_WIDTH.W))
  val phyRegs = Module(new PhyRegs(N_ARCH_REGISTERS))

  val fetch = Module(new Fetch(ADDR_WIDTH, INST_WIDTH))
  val decode = Module(new Decode(INST_WIDTH, DATA_WIDTH))
  val execute = Module(new Execute(DATA_WIDTH))
  val memory = Module(new Memory(ADDR_WIDTH, DATA_WIDTH))
  val writeBack = Module(new WriteBack(DATA_WIDTH))

  val fetchedInst = Wire(UInt(INST_WIDTH.W))

  // checks
  assert(N_ARCH_REGISTERS == Decode.N_ARCH_REGISTERS)

  // fetch
  fetch.io.pc := pc
  fetch.io.iReadAddr <> io.iReadAddr
  fetch.io.iReadValue <> io.iReadValue

  // decode
  decode.io.inst := fetchedInst
  phyRegs.io.rs1 := decode.io.signals.rs1
  phyRegs.io.rs2 := decode.io.signals.rs2
  phyRegs.io.rd := decode.io.signals.rd
  phyRegs.io.rdEn := decode.io.signals.regWrite

  // execute
  execute.io.op := decode.io.signals.aluOp
  execute.io.bNot := decode.io.signals.bNot
  execute.io.a := phyRegs.io.rs1Value
  execute.io.b := phyRegs.io.rs2Value

  // memory
  memory.io.address := execute.io.result
  memory.io.writeData := phyRegs.io.rs2Value
  memory.io.writeEn := decode.io.signals.memWrite
  memory.io.readEn := decode.io.signals.memRead

  // write-back
  writeBack.io.memToReg := decode.io.signals.memToReg
  writeBack.io.execResult := execute.io.result
  writeBack.io.readData := memory.io.readData

  phyRegs.io.rdValue := writeBack.io.execResult

  when(fetch.io.inst.valid) {
    fetchedInst := fetch.io.inst.deq()
    pc := Mux(execute.io.zero && decode.io.signals.branch, pc + decode.io.signals.immediate, pc + 4.U)
  }.otherwise {
    fetch.io.inst.nodeq()
    fetchedInst := NOP.U
  }
}
