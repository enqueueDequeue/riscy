package io.riscy

import chisel3.util.Decoupled
import chisel3.{Bundle, DontCare, Flipped, Input, Module, Mux, Output, PrintableHelper, RegInit, UInt, Wire, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.signals.Parameters
import io.riscy.stages.signals.Utils.NOP
import io.riscy.stages.{Decode, Execute, Fetch, Memory, PhyRegs, WriteBack}

class InOrderCPU()(implicit val params: Parameters) extends Module {
  // The magic instruction
  // Decode to find why this is the NOP
  val bitWidth = params.bitWidth
  val nArchRegs = params.nArchRegs
  val addrWidth = params.addrWidth
  val dataWidth = params.dataWidth
  val instWidth = params.instWidth

  val io = IO(new Bundle {
    val iReadAddr = Decoupled(UInt(addrWidth.W))
    val iReadValue = Flipped(Decoupled(UInt(instWidth.W)))

    val dReadLen = Output(UInt((dataWidth / bitWidth).W))
    val dReadAddr = Output(UInt(addrWidth.W))
    val dReadValue = Input(UInt(dataWidth.W))

    val dWriteLen = Output(UInt((dataWidth / bitWidth).W))
    val dWriteAddr = Output(UInt(addrWidth.W))
    val dWriteValue = Output(UInt(dataWidth.W))
  })

  val pc = RegInit(0.U(addrWidth.W))
  val nextPc = pc + 4.U

  val phyRegs = Module(new PhyRegs())

  val fetch = Module(new Fetch())
  val decode = Module(new Decode())
  val execute = Module(new Execute())
  val memory = Module(new Memory())
  val writeBack = Module(new WriteBack())

  printf(cf"pc: $pc\n")
  printf(cf"signals: ${decode.io.signals}\n")
  printf(cf"a: ${execute.io.a}, b: ${execute.io.b}, result: ${execute.io.result}\n")
  printf(cf"wb: ${writeBack.io.result}%x\n")

  val fetchedInst = Wire(UInt(instWidth.W))

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
  phyRegs.io.rd2 := 0.U
  phyRegs.io.rd2En := false.B
  phyRegs.io.rd2Value := DontCare

  val rs1Value = Mux(decode.io.signals.rs1 === 0.U, 0.U, phyRegs.io.rs1Value)
  val rs2Value = Mux(decode.io.signals.rs2 === 0.U, 0.U, phyRegs.io.rs2Value)

  // execute
  execute.io.word := decode.io.signals.word
  execute.io.op := decode.io.signals.aluOp
  execute.io.branchInvert := decode.io.signals.branchInvert
  execute.io.a := Mux(decode.io.signals.rs1Pc, pc, rs1Value)
  execute.io.b := Mux(decode.io.signals.rs2Imm, decode.io.signals.immediate, rs2Value)

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
    fetchedInst := fetch.io.inst.bits

    when(decode.io.signals.jump) {
      pc := execute.io.result
    }.otherwise {
      pc := Mux(execute.io.zero && decode.io.signals.branch, pc + decode.io.signals.immediate, nextPc)
    }
  }.otherwise {
    fetchedInst := NOP.U
  }
}
