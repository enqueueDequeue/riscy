package io.riscy

import chisel3.util.Decoupled
import chisel3.{Bundle, Flipped, Input, Module, Mux, Output, PrintableHelper, RegInit, UInt, Wire, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.{Decode, Execute, Fetch, Memory, PhyRegs, WriteBack}

class InOrderCPU extends Module {
  // The magic instruction
  // Decode to find why this is the NOP
  val NOP = 0x33

  val BIT_WIDTH = 8
  val N_ARCH_REGISTERS = 32
  val ARCH_WIDTH = 64
  val ADDR_WIDTH = ARCH_WIDTH
  val DATA_WIDTH = ARCH_WIDTH
  val INST_WIDTH = 32

  val io = IO(new Bundle {
    val iReadAddr = Decoupled(UInt(ADDR_WIDTH.W))
    val iReadValue = Flipped(Decoupled(UInt(INST_WIDTH.W)))

    // todo: make these decoupled
    val dReadLen = Output(UInt((DATA_WIDTH / BIT_WIDTH).W))
    val dReadAddr = Output(UInt(ADDR_WIDTH.W))
    val dReadValue = Input(UInt(DATA_WIDTH.W))

    val dWriteLen = Output(UInt((DATA_WIDTH / BIT_WIDTH).W))
    val dWriteAddr = Output(UInt(ADDR_WIDTH.W))
    val dWriteValue = Output(UInt(DATA_WIDTH.W))
  })

  val pc = RegInit(0.U(ARCH_WIDTH.W))
  val nextPc = pc + 4.U

  val phyRegs = Module(new PhyRegs(N_ARCH_REGISTERS))

  val fetch = Module(new Fetch(ADDR_WIDTH, INST_WIDTH))
  val decode = Module(new Decode(INST_WIDTH, DATA_WIDTH))
  val execute = Module(new Execute(DATA_WIDTH))
  val memory = Module(new Memory(ADDR_WIDTH, DATA_WIDTH))
  val writeBack = Module(new WriteBack(DATA_WIDTH))

  printf(cf"pc: $pc\n")
  printf(cf"signals: ${decode.io.signals}\n")
  printf(cf"a: ${execute.io.a}, b: ${execute.io.b}, result: ${execute.io.result}\n")
  printf(cf"wb: ${writeBack.io.result}%x\n")

  val fetchedInst = Wire(UInt(INST_WIDTH.W))

  // setup
  memory.io.dReadLen <> io.dReadLen
  memory.io.dReadAddr <> io.dReadAddr
  memory.io.dReadValue <> io.dReadValue
  memory.io.dWriteLen <> io.dWriteLen
  memory.io.dWriteAddr <> io.dWriteAddr
  memory.io.dWriteValue <> io.dWriteValue

  fetch.io.iReadAddr <> io.iReadAddr
  fetch.io.iReadValue <> io.iReadValue

  // fetch
  fetch.io.pc := pc

  // decode
  decode.io.inst := fetchedInst
  phyRegs.io.rs1 := decode.io.signals.rs1
  phyRegs.io.rs2 := decode.io.signals.rs2
  phyRegs.io.rd := decode.io.signals.rd
  phyRegs.io.rdEn := decode.io.signals.regWrite

  // execute
  execute.io.word := decode.io.signals.word
  execute.io.op := decode.io.signals.aluOp
  execute.io.branchInvert := decode.io.signals.branchInvert
  execute.io.a := Mux(decode.io.signals.rs1Pc, pc, phyRegs.io.rs1Value)
  execute.io.b := Mux(decode.io.signals.rs2Imm, decode.io.signals.immediate, phyRegs.io.rs2Value)

  // memory
  memory.io.address := execute.io.result
  memory.io.writeData := phyRegs.io.rs2Value
  memory.io.writeSize := decode.io.signals.memWrite
  memory.io.readSize := decode.io.signals.memRead

  // write-back
  writeBack.io.memToReg := decode.io.signals.memToReg
  writeBack.io.execResult := Mux(decode.io.signals.jump, nextPc, execute.io.result)
  writeBack.io.readData := memory.io.readData

  phyRegs.io.rdValue := writeBack.io.result

  // control
  when(fetch.io.inst.valid) {
    fetchedInst := fetch.io.inst.deq()

    when(decode.io.signals.jump) {
      pc := execute.io.result
    }.otherwise {
      pc := Mux(execute.io.zero && decode.io.signals.branch, pc + decode.io.signals.immediate, nextPc)
    }
  }.otherwise {
    fetch.io.inst.nodeq()
    fetchedInst := NOP.U
  }
}
