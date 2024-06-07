package io.riscy

import chisel3.util.Decoupled
import chisel3.{Bundle, Data, DontCare, Flipped, Input, Module, Mux, Output, PrintableHelper, RegInit, UInt, Wire, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.InOrderPipelinedCPU.{ADDR_WIDTH, ARCH_WIDTH, BIT_WIDTH, DATA_WIDTH, INST_WIDTH, NOP, N_ARCH_REGISTERS, PC_INIT, initDecodeSignals, initExecuteSignals, initFetchSignals, initMemorySignals, initRegReadSignals, initStage}
import io.riscy.stages.signals._
import io.riscy.stages._

class InOrderPipelinedCPU extends Module {

  val io = IO(new Bundle {
    val iReadAddr = Decoupled(UInt(ADDR_WIDTH.W))
    val iReadValue = Flipped(Decoupled(UInt(INST_WIDTH.W)))

    val dReadLen = Output(UInt((DATA_WIDTH / BIT_WIDTH).W))
    val dReadAddr = Output(UInt(ADDR_WIDTH.W))
    val dReadValue = Input(UInt(DATA_WIDTH.W))

    val dWriteLen = Output(UInt((DATA_WIDTH / BIT_WIDTH).W))
    val dWriteAddr = Output(UInt(ADDR_WIDTH.W))
    val dWriteValue = Output(UInt(DATA_WIDTH.W))
  })

  val fetchSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initSignals
  })

  val decodeSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val decode = DecodeSignals()
    }))
    initStage(initSignals)
    initDecodeSignals(initSignals.stage.decode)
    initSignals
  })

  val regReadSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val decode = DecodeSignals()
      val regRead = RegReadSignals()
    }))
    initStage(initSignals)
    initDecodeSignals(initSignals.stage.decode)
    initRegReadSignals(initSignals.stage.regRead)
    initSignals
  })

  val executeSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val decode = DecodeSignals()
      val regRead = RegReadSignals()
      val execute = ExecuteSignals()
    }))
    initStage(initSignals)
    initDecodeSignals(initSignals.stage.decode)
    initRegReadSignals(initSignals.stage.regRead)
    initExecuteSignals(initSignals.stage.execute)
    initSignals
  })

  val memorySignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val decode = DecodeSignals()
      val regRead = RegReadSignals()
      val execute = ExecuteSignals()
      val memory = MemorySignals()
    }))
    initStage(initSignals)
    initDecodeSignals(initSignals.stage.decode)
    initRegReadSignals(initSignals.stage.regRead)
    initExecuteSignals(initSignals.stage.execute)
    initMemorySignals(initSignals.stage.memory)
    initSignals
  })

  val pc = RegInit(PC_INIT.U(ARCH_WIDTH.W))
  val nextPc = pc + 4.U

  val phyRegs = Module(new PhyRegs(N_ARCH_REGISTERS))

  printf(cf"pc: $pc\n")

  // checks
  assert(N_ARCH_REGISTERS == Decode.N_ARCH_REGISTERS)
  assert(BIT_WIDTH == Memory.BIT_WIDTH)
  assert(INST_WIDTH == Defaults.INST_WIDTH)

  // fetch
  {
    val fetch = Module(new Fetch(ADDR_WIDTH, INST_WIDTH))
    val fetchSignalsWire = Wire(FetchSignals())

    fetch.io.iReadAddr <> io.iReadAddr
    fetch.io.iReadValue <> io.iReadValue

    fetch.io.pc := pc

    when(fetch.io.inst.valid) {
      fetchSignalsWire.instruction.valid := true.B
      fetchSignalsWire.instruction.bits := fetch.io.inst.deq()
    }.otherwise {
      fetchSignalsWire.instruction.valid := false.B
      fetchSignalsWire.instruction.bits := DontCare
      fetch.io.inst.nodeq()
    }

    printf(cf"fetchSignals: $fetchSignalsWire\n")

    fetchSignals.pc := Mux(fetch.io.inst.valid, pc, PC_INIT.U)
    fetchSignals.stage.fetch := fetchSignalsWire
  }

  // decode
  {
    val decode = Module(new Decode(INST_WIDTH, DATA_WIDTH))
    val decodeSignalsWire = Wire(DecodeSignals())

    decode.io.inst := Mux(fetchSignals.stage.fetch.instruction.valid,
                          fetchSignals.stage.fetch.instruction.bits, NOP.U)

    decodeSignalsWire := decode.io.signals

    printf(cf"decodeSignals: $decodeSignalsWire\n")

    decodeSignals.pc := fetchSignals.pc
    decodeSignals.stage.decode := decodeSignalsWire
  }

  // Register Read
  {
    val regReadSignalsWire = Wire(RegReadSignals())

    phyRegs.io.rs1 := decodeSignals.stage.decode.rs1
    phyRegs.io.rs2 := decodeSignals.stage.decode.rs2
    phyRegs.io.rd := decodeSignals.stage.decode.rd
    phyRegs.io.rdEn := decodeSignals.stage.decode.regWrite

    regReadSignalsWire.rs1Value := phyRegs.io.rs1Value
    regReadSignalsWire.rs2Value := phyRegs.io.rs2Value

    printf(cf"regReadSignals: $regReadSignalsWire\n")

    regReadSignals.pc := decodeSignals.pc
    regReadSignals.stage.decode := decodeSignals.stage.decode
    regReadSignals.stage.regRead := regReadSignalsWire

    // todo: implement stalling
  }

  // execute
  {
    val execute = Module(new Execute(DATA_WIDTH))
    val executeSignalsWire = Wire(ExecuteSignals())

    execute.io.op := regReadSignals.stage.decode.aluOp
    execute.io.branchInvert := regReadSignals.stage.decode.branchInvert

    // todo: implement forwarding
    execute.io.a := Mux(regReadSignals.stage.decode.rs1Pc, regReadSignals.pc, regReadSignals.stage.regRead.rs1Value)
    execute.io.b := Mux(regReadSignals.stage.decode.rs2Imm, regReadSignals.stage.decode.immediate, regReadSignals.stage.regRead.rs2Value)

    executeSignalsWire.result := execute.io.result
    executeSignalsWire.zero := execute.io.zero

    printf(cf"executeSignals: $executeSignalsWire\n")

    executeSignals.pc := regReadSignals.pc
    executeSignals.stage.decode := regReadSignals.stage.decode
    executeSignals.stage.regRead := regReadSignals.stage.regRead
    executeSignals.stage.execute := executeSignalsWire
  }

  // memory
  {
    val memory = Module(new Memory(ADDR_WIDTH, DATA_WIDTH))

    // setup
    memory.io.dReadLen <> io.dReadLen
    memory.io.dReadAddr <> io.dReadAddr
    memory.io.dReadValue <> io.dReadValue
    memory.io.dWriteLen <> io.dWriteLen
    memory.io.dWriteAddr <> io.dWriteAddr
    memory.io.dWriteValue <> io.dWriteValue

    val memorySignalsWire = Wire(MemorySignals())

    memory.io.address := executeSignals.stage.execute.result
    memory.io.writeData := phyRegs.io.rs2Value
    memory.io.writeSize := executeSignals.stage.decode.memWrite
    memory.io.readSize := executeSignals.stage.decode.memRead

    memorySignalsWire.readData := memory.io.readData

    memorySignals.pc := executeSignals.pc
    memorySignals.stage.decode := executeSignals.stage.decode
    memorySignals.stage.regRead := executeSignals.stage.regRead
    memorySignals.stage.execute := executeSignals.stage.execute
    memorySignals.stage.memory := memorySignalsWire
  }

  // write-back
  {
    val writeBack = Module(new WriteBack(DATA_WIDTH))

    writeBack.io.memToReg := memorySignals.stage.decode.memToReg
    writeBack.io.execResult := Mux(memorySignals.stage.decode.jump, nextPc, memorySignals.stage.execute.result)
    writeBack.io.readData := memorySignals.stage.memory.readData

    phyRegs.io.rdEn := memorySignals.stage.decode.regWrite
    phyRegs.io.rdValue := writeBack.io.result

    printf(cf"wb: ${writeBack.io.result}%x\n")
  }

  // control
  when(executeSignals.stage.decode.jump) {
    pc := executeSignals.stage.execute.result
    // todo: kill the Execute, RegRead, Decode
    //  and Fetch stage instructions
  }.elsewhen(executeSignals.stage.execute.zero && executeSignals.stage.decode.branch) {
    pc := pc + executeSignals.stage.decode.immediate
    // todo: kill the Execute, RegRead, Decode
    //  and Fetch stage instructions
  }.elsewhen(fetchSignals.stage.fetch.instruction.valid) {
    pc := nextPc
  }
}

object InOrderPipelinedCPU {
  // The magic NOP
  // Decode to find why this is the NOP
  val NOP = 0x33

  val PC_INIT = 0

  val BIT_WIDTH = 8
  val N_ARCH_REGISTERS = 32
  val ARCH_WIDTH = 64
  val ADDR_WIDTH = ARCH_WIDTH
  val DATA_WIDTH = ARCH_WIDTH
  val INST_WIDTH = 32

  private def initStage[T <: Data](stage: Stage[T]) = {
    stage.pc := PC_INIT.U
  }

  private def initFetchSignals(fetchSignals: FetchSignals) = {
    fetchSignals.instruction.valid := false.B
    fetchSignals.instruction.bits := NOP.U
  }

  private def initDecodeSignals(decodeSignals: DecodeSignals) = {
    decodeSignals.jump := false.B
    decodeSignals.branch := false.B
    decodeSignals.memToReg := false.B
    decodeSignals.memRead := MemRWSize.BYTES_NO
    decodeSignals.memWrite := MemRWSize.BYTES_NO
    decodeSignals.rs1Pc := false.B
    decodeSignals.rs2Imm := false.B
    decodeSignals.regWrite := false.B
    decodeSignals.branchInvert := false.B
    decodeSignals.aluOp := ExecuteOp.NOP
    decodeSignals.immediate := false.B
    decodeSignals.rs1 := 0.U
    decodeSignals.rs2 := 0.U
    decodeSignals.rd := 0.U
  }

  private def initRegReadSignals(regReadSignals: RegReadSignals) = {
    regReadSignals.rs1Value := 0.U
    regReadSignals.rs2Value := 0.U
  }

  private def initExecuteSignals(executeSignals: ExecuteSignals) = {
    executeSignals.result := 0.U
    executeSignals.zero := false.B
  }

  private def initMemorySignals(memorySignals: MemorySignals) = {
    memorySignals.readData := 0.U
  }
}
