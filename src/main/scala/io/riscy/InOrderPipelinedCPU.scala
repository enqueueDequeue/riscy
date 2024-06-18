package io.riscy

import chisel3.util.Decoupled
import chisel3.{Bool, Bundle, Data, DontCare, Flipped, Input, Module, Mux, Output, PrintableHelper, RegInit, UInt, Wire, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.InOrderPipelinedCPU.{ADDR_WIDTH, ARCH_WIDTH, BIT_WIDTH, DATA_WIDTH, INST_WIDTH, NOP, N_ARCH_REGISTERS, PC_INIT, forward, initDecodeSignals, initExecuteSignals, initFetchSignals, initMemorySignals, initRegReadSignals, initStage}
import io.riscy.stages.signals._
import io.riscy.stages._

/**
 * An InOrder 6 Stage Processor
 *
 * Fetch -> Decode -> Register Read -> Execute -> Memory -> WriteBack
 */
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
      val fetch = FetchSignals()
      val decode = DecodeSignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initDecodeSignals(initSignals.stage.decode)
    initSignals
  })

  val regReadSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
      val decode = DecodeSignals()
      val regRead = RegReadSignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initDecodeSignals(initSignals.stage.decode)
    initRegReadSignals(initSignals.stage.regRead)
    initSignals
  })

  val executeSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
      val decode = DecodeSignals()
      val regRead = RegReadSignals()
      val execute = ExecuteSignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initDecodeSignals(initSignals.stage.decode)
    initRegReadSignals(initSignals.stage.regRead)
    initExecuteSignals(initSignals.stage.execute)
    initSignals
  })

  val memorySignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
      val decode = DecodeSignals()
      val regRead = RegReadSignals()
      val execute = ExecuteSignals()
      val memory = MemorySignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initDecodeSignals(initSignals.stage.decode)
    initRegReadSignals(initSignals.stage.regRead)
    initExecuteSignals(initSignals.stage.execute)
    initMemorySignals(initSignals.stage.memory)
    initSignals
  })

  // Used to froward stuff
  val fetchSignalsWire = Wire(FetchSignals())
  val decodeSignalsWire = Wire(DecodeSignals())
  val regReadSignalsWire = Wire(RegReadSignals())
  val executeSignalsWire = Wire(ExecuteSignals())
  val memorySignalsWire = Wire(MemorySignals())

  // Indicates if the instruction needs to be stalled
  val stall = Wire(Bool())
  val pc = RegInit(PC_INIT.U(ARCH_WIDTH.W))

  val phyRegs = Module(new PhyRegs(N_ARCH_REGISTERS))

  printf(cf"pc: $pc\n")

  // checks
  assert(N_ARCH_REGISTERS == Decode.N_ARCH_REGISTERS)
  assert(BIT_WIDTH == Memory.BIT_WIDTH)
  assert(INST_WIDTH == Defaults.INST_WIDTH)

  stall := false.B

  // fetch
  {
    val fetch = Module(new Fetch(ADDR_WIDTH, INST_WIDTH))

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

    when(!stall) {
      printf(cf"fetchSignals: valid: ${fetchSignalsWire.instruction.valid}%x, inst: ${fetchSignalsWire.instruction.bits}%x\n")

      fetchSignals.pc := Mux(fetch.io.inst.valid, pc, PC_INIT.U)
      fetchSignals.stage.fetch := fetchSignalsWire
    }.otherwise {
      printf(cf"fetchSignals: valid: ${fetchSignalsWire.instruction.valid}%x, Stalling\n")

      fetchSignals.pc := PC_INIT.U
      fetchSignals.stage.fetch.instruction.valid := false.B
      fetchSignals.stage.fetch.instruction.bits := NOP.U
    }
  }

  // decode
  {
    val decode = Module(new Decode(INST_WIDTH, DATA_WIDTH))

    decode.io.inst := Mux(fetchSignals.stage.fetch.instruction.valid,
                          fetchSignals.stage.fetch.instruction.bits, NOP.U)

    decodeSignalsWire := decode.io.signals

    when(!stall) {
      printf(cf"decodeInst: ${fetchSignals.stage.fetch.instruction.bits}%x decodeSignals: $decodeSignalsWire\n")

      decodeSignals.pc := fetchSignals.pc
      decodeSignals.stage.fetch := fetchSignals.stage.fetch
      decodeSignals.stage.decode := decodeSignalsWire
    }.otherwise {
      printf(cf"decodeInst: ${fetchSignals.stage.fetch.instruction.bits}%x Stalling\n")

      // let all the signals pass as it is
      // except for the ones that effect the state of the system
      decodeSignals.pc := fetchSignals.pc
      decodeSignals.stage.fetch := fetchSignals.stage.fetch
      decodeSignals.stage.decode := decodeSignalsWire

      decodeSignals.stage.decode.memRead := MemRWSize.BYTES_NO
      decodeSignals.stage.decode.memWrite := MemRWSize.BYTES_NO
      decodeSignals.stage.decode.regWrite := false.B
      decodeSignals.stage.decode.branch := false.B
      decodeSignals.stage.decode.jump := false.B
    }
  }

  // Register Read
  {
    val stallRs1 = Wire(Bool())
    val stallRs2 = Wire(Bool())

    phyRegs.io.rs1 := decodeSignals.stage.decode.rs1
    phyRegs.io.rs2 := decodeSignals.stage.decode.rs2

    printf(cf"rs1: ${decodeSignals.stage.decode.rs1} rs2: ${decodeSignals.stage.decode.rs2} rd: ${decodeSignals.stage.decode.rd}\n")

    printf(cf"rs1:\n")
    forward(decodeSignals.stage.decode.rs1,
            phyRegs.io.rs1Value,
            executeSignalsWire, memorySignalsWire,
            regReadSignals.stage.decode, executeSignals.stage.decode,
            executeSignals.stage.execute,
            regReadSignalsWire.rs1Value, stallRs1)

    printf(cf"rs2:\n")
    forward(decodeSignals.stage.decode.rs2,
            phyRegs.io.rs2Value,
            executeSignalsWire, memorySignalsWire,
            regReadSignals.stage.decode, executeSignals.stage.decode,
            executeSignals.stage.execute,
            regReadSignalsWire.rs2Value, stallRs2)

    stall := stallRs1 | stallRs2

    // Writing to regReadSignals
    // NOTE: NO write MUST be performed on regReadSignals before this point
    when(!stall) {
      printf(cf"regReadInst: ${decodeSignals.stage.fetch.instruction.bits}%x regReadSignals: $regReadSignalsWire\n")

      regReadSignals.pc := decodeSignals.pc
      regReadSignals.stage.fetch := decodeSignals.stage.fetch
      regReadSignals.stage.decode := decodeSignals.stage.decode
      regReadSignals.stage.regRead := regReadSignalsWire
    }.otherwise {
      printf(cf"regReadInst: ${decodeSignals.stage.fetch.instruction.bits}%x -> regReadSignals: Stalling\n")

      // passing stuff as it is except for a few changes

      regReadSignals.pc := decodeSignals.pc
      regReadSignals.stage.fetch := decodeSignals.stage.fetch
      regReadSignals.stage.decode := decodeSignals.stage.decode
      regReadSignals.stage.regRead := regReadSignalsWire

      regReadSignals.stage.decode.memRead := MemRWSize.BYTES_NO
      regReadSignals.stage.decode.memWrite := MemRWSize.BYTES_NO
      regReadSignals.stage.decode.regWrite := false.B
      regReadSignals.stage.decode.branch := false.B
      regReadSignals.stage.decode.jump := false.B
    }
  }

  // execute
  {
    val execute = Module(new Execute(DATA_WIDTH))

    execute.io.op := regReadSignals.stage.decode.aluOp
    execute.io.branchInvert := regReadSignals.stage.decode.branchInvert

    execute.io.a := Mux(regReadSignals.stage.decode.rs1Pc, regReadSignals.pc, regReadSignals.stage.regRead.rs1Value)
    execute.io.b := Mux(regReadSignals.stage.decode.rs2Imm, regReadSignals.stage.decode.immediate, regReadSignals.stage.regRead.rs2Value)

    executeSignalsWire.result := Mux(regReadSignals.stage.decode.jump, regReadSignals.pc + 4.U, execute.io.result)
    executeSignalsWire.zero := execute.io.zero

    printf(cf"executeInst: ${regReadSignals.stage.fetch.instruction.bits}%x executeSignals: $executeSignalsWire\n")

    executeSignals.pc := regReadSignals.pc
    executeSignals.stage.fetch := regReadSignals.stage.fetch
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

    memory.io.address := executeSignals.stage.execute.result
    memory.io.writeData := executeSignals.stage.regRead.rs2Value
    memory.io.writeSize := executeSignals.stage.decode.memWrite
    memory.io.readSize := executeSignals.stage.decode.memRead

    memorySignalsWire.readData := memory.io.readData

    memorySignals.pc := executeSignals.pc
    memorySignals.stage.fetch := executeSignals.stage.fetch
    memorySignals.stage.decode := executeSignals.stage.decode
    memorySignals.stage.regRead := executeSignals.stage.regRead
    memorySignals.stage.execute := executeSignals.stage.execute
    memorySignals.stage.memory := memorySignalsWire

    printf(cf"memoryInst: ${executeSignals.stage.fetch.instruction.bits}%x memorySignals: $memorySignalsWire\n")
  }

  // write-back
  {
    val writeBack = Module(new WriteBack(DATA_WIDTH))

    writeBack.io.memToReg := memorySignals.stage.decode.memToReg
    writeBack.io.execResult := memorySignals.stage.execute.result
    writeBack.io.readData := memorySignals.stage.memory.readData

    phyRegs.io.rd := memorySignals.stage.decode.rd
    phyRegs.io.rdEn := memorySignals.stage.decode.regWrite
    phyRegs.io.rdValue := writeBack.io.result

    printf(cf"wbInst: ${memorySignals.stage.fetch.instruction.bits}%x wb: ${writeBack.io.result}%x\n")
  }

  // control
  // Always, check the signals that execute is working on
  // OR the memory stage is working on
  when(regReadSignals.stage.decode.jump) {
    pc := executeSignalsWire.result
    // todo: kill the Execute, RegRead, Decode
    //  and Fetch stage instructions
  }.elsewhen(executeSignalsWire.zero && regReadSignals.stage.decode.branch) {
    pc := pc + regReadSignals.stage.decode.immediate
    // todo: kill the Execute, RegRead, Decode
    //  and Fetch stage instructions
  }.elsewhen(fetchSignalsWire.instruction.valid) {
    pc := pc + 4.U
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

  /**
   * forward the values appropriately
   * @param rs The source register whose value has to be forwarded
   * @param executeSignalsWire The results of the current execute stage
   * @param regReadSignalsDecode The register values of the regReadSignal's decode stage, this is what the execute stage is being fed in the current clock
   * @param executeSignalsDecode The register values of executeSignal's decode stage, this is what the memory stage is being fed in the current clock
   * @param executeSignals The register values of executeSignal's execute stage, this is also what the memory stage is being fed in the current clock
   * @param out The contents which have to be forwarded
   * @param stallOut The stall flag which has to be used
   */
  private def forward(rs: UInt,
                      rsValue: UInt,
                      executeSignalsWire: ExecuteSignals,
                      memorySignalsWire: MemorySignals,
                      regReadSignalsDecode: DecodeSignals,
                      executeSignalsDecode: DecodeSignals,
                      executeSignals: ExecuteSignals,
                      out: UInt, stallOut: Bool): Unit = {

    // if any of the future stages have computed the values of
    // the source register, then forward it
    when(0.U === rs) {
      printf(cf"rs = 0\n")

      out := 0.U
      stallOut := false.B
    }.elsewhen(regReadSignalsDecode.rd === rs) {
      // if the regReadSignals is holding the value
      // that means the execute is working on the instruction

      printf(cf"rs: forwarding execute result\n")

      // Using memToReg to stall an instruction
      // In this architecture, the ALU is a
      // single cycle stage. All the instructions in
      // the ALU  will be performed within a clock
      // So, the only possible way to stall an instruction
      // is through memory instructions which cannot forward
      // the results until the memory stage completes

      out := executeSignalsWire.result

      // stall whenever there's a memory read
      stallOut := regReadSignalsDecode.memToReg
    }.elsewhen(executeSignalsDecode.rd === rs) {
      // if the executeSignals is holding the value
      // that means the memory stage is working on the instruction

      printf(cf"rs: forwarding memory result\n")

      out := Mux(executeSignalsDecode.memToReg, memorySignalsWire.readData, executeSignals.result)
      stallOut := false.B
    }.otherwise {
      printf(cf"rs: reading the register\n")

      out := rsValue
      stallOut := false.B
    }
  }
}
