package io.riscy

import chisel3.util.Decoupled
import chisel3.{Bool, Bundle, Flipped, Input, Module, Mux, Output, PrintableHelper, RegInit, UInt, Wire, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.InOrderPipelinedCPU.forward
import io.riscy.stages.{Decode, Execute, Fetch, Memory, PhyRegs, WriteBack}
import io.riscy.stages.signals.{DecodeSignals, ExecuteSignals, FetchSignals, MemorySignals, Parameters, RegReadSignals, Stage, WriteBackSignals}
import io.riscy.stages.signals.Utils.{NOP, PC_INIT, initDecodeSignals, initExecuteSignals, initFetchSignals, initMemorySignals, initRegReadSignals, initStage, killDecodeSignals, killFetchSignals}

/**
 * An InOrder 6 Stage Processor
 *
 * Fetch -> Decode -> Register Read -> Execute -> Memory -> WriteBack
 */
class InOrderPipelinedCPU()(implicit val params: Parameters) extends Module {
  val addrWidth = params.addrWidth
  val bitWidth = params.bitWidth
  val instWidth = params.instWidth
  val dataWidth = params.dataWidth

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

  val ifIdSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
    }))

    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initSignals
  })

  val idRrSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
      val decode = DecodeSignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initDecodeSignals(initSignals.stage.decode)
    initSignals
  })

  val rrExSignals = RegInit({
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

  val exMemSignals = RegInit({
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

  val memWbSignals = RegInit({
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
  val writeBackSignalsWire = Wire(WriteBackSignals())

  // Indicates if the instruction needs to be stalled
  val stall = Wire(Bool())
  val pc = RegInit(PC_INIT.U(addrWidth.W))

  val phyRegs = Module(new PhyRegs())

  printf(cf"pc: $pc\n")

  stall := false.B

  // fetch
  {
    val fetch = Module(new Fetch())

    fetch.io.iReadAddr <> io.iReadAddr
    fetch.io.iReadValue <> io.iReadValue

    fetch.io.pc := pc

    when(fetch.io.inst.valid) {
      fetchSignalsWire.instruction.valid := true.B
      fetchSignalsWire.instruction.bits := fetch.io.inst.deq()
    }.otherwise {
      fetchSignalsWire.instruction.valid := false.B
      fetchSignalsWire.instruction.bits := NOP.U
      fetch.io.inst.nodeq()
    }

    when(!stall) {
      printf(cf"fetchSignals: valid: ${fetchSignalsWire.instruction.valid}%x, inst: ${fetchSignalsWire.instruction.bits}%x\n")

      ifIdSignals.pc := Mux(fetchSignalsWire.instruction.valid, pc, PC_INIT.U)
      ifIdSignals.stage.fetch := fetchSignalsWire
    }
  }

  // decode
  {
    val decode = Module(new Decode())

    decode.io.inst := ifIdSignals.stage.fetch.instruction.bits

    decodeSignalsWire := decode.io.signals

    when(!stall) {
      printf(cf"decodeInst: ${ifIdSignals.stage.fetch.instruction.bits}%x decodeSignals: $decodeSignalsWire\n")

      idRrSignals.pc := ifIdSignals.pc
      idRrSignals.stage.fetch := ifIdSignals.stage.fetch
      idRrSignals.stage.decode := decodeSignalsWire
    }
  }

  // Register Read
  {
    val stallRs1 = Wire(Bool())
    val stallRs2 = Wire(Bool())

    phyRegs.io.rs1 := idRrSignals.stage.decode.rs1
    phyRegs.io.rs2 := idRrSignals.stage.decode.rs2

    printf(cf"rs1: ${idRrSignals.stage.decode.rs1} rs2: ${idRrSignals.stage.decode.rs2} rd: ${idRrSignals.stage.decode.rd}\n")

    printf(cf"rs1:\n")
    forward(idRrSignals.stage.decode.rs1,
            phyRegs.io.rs1Value,
            executeSignalsWire, memorySignalsWire, writeBackSignalsWire,
            rrExSignals.stage.decode, exMemSignals.stage.decode, memWbSignals.stage.decode,
            exMemSignals.stage.execute,
            regReadSignalsWire.rs1Value, stallRs1)

    printf(cf"rs2:\n")
    forward(idRrSignals.stage.decode.rs2,
            phyRegs.io.rs2Value,
            executeSignalsWire, memorySignalsWire, writeBackSignalsWire,
            rrExSignals.stage.decode, exMemSignals.stage.decode, memWbSignals.stage.decode,
            exMemSignals.stage.execute,
            regReadSignalsWire.rs2Value, stallRs2)

    stall := stallRs1 | stallRs2

    // Writing to regReadSignals
    // NOTE: writes *MUST NOT* be performed on regReadSignals before this point
    when(!stall) {
      printf(cf"regReadInst: ${idRrSignals.stage.fetch.instruction.bits}%x regReadSignals: $regReadSignalsWire\n")

      rrExSignals.pc := idRrSignals.pc
      rrExSignals.stage.fetch := idRrSignals.stage.fetch
      rrExSignals.stage.decode := idRrSignals.stage.decode
      rrExSignals.stage.regRead := regReadSignalsWire
    }.otherwise {
      printf(cf"regReadInst: ${idRrSignals.stage.fetch.instruction.bits}%x -> regReadSignals: Stalling\n")

      // passing stuff as it is except for a few changes

      rrExSignals.pc := idRrSignals.pc
      rrExSignals.stage.fetch := idRrSignals.stage.fetch
      rrExSignals.stage.decode := idRrSignals.stage.decode
      rrExSignals.stage.regRead := regReadSignalsWire

      killDecodeSignals(rrExSignals.stage.decode)
    }
  }

  // execute
  {
    val execute = Module(new Execute())

    execute.io.op := rrExSignals.stage.decode.aluOp
    execute.io.branchInvert := rrExSignals.stage.decode.branchInvert
    execute.io.word := rrExSignals.stage.decode.word

    execute.io.a := Mux(rrExSignals.stage.decode.rs1Pc, rrExSignals.pc, rrExSignals.stage.regRead.rs1Value)
    execute.io.b := Mux(rrExSignals.stage.decode.rs2Imm, rrExSignals.stage.decode.immediate, rrExSignals.stage.regRead.rs2Value)

    // In case of Jump instructions:
    // ALU calculates pc + imm. But, the results are always ignored
    // by the following mux. The nextPc computed will be used to set the PC
    executeSignalsWire.nextPc := Mux(rrExSignals.stage.decode.jump, execute.io.result, rrExSignals.pc + rrExSignals.stage.decode.immediate)
    executeSignalsWire.result := Mux(rrExSignals.stage.decode.jump, rrExSignals.pc + 4.U, execute.io.result)
    executeSignalsWire.zero := execute.io.zero

    printf(cf"executeInst: ${rrExSignals.stage.fetch.instruction.bits}%x executeSignals: $executeSignalsWire\n")

    exMemSignals.pc := rrExSignals.pc
    exMemSignals.stage.fetch := rrExSignals.stage.fetch
    exMemSignals.stage.decode := rrExSignals.stage.decode
    exMemSignals.stage.regRead := rrExSignals.stage.regRead
    exMemSignals.stage.execute := executeSignalsWire
  }

  // memory
  {
    val memory = Module(new Memory())

    // setup
    memory.io.dReadLen <> io.dReadLen
    memory.io.dReadAddr <> io.dReadAddr
    memory.io.dReadValue <> io.dReadValue
    memory.io.dWriteLen <> io.dWriteLen
    memory.io.dWriteAddr <> io.dWriteAddr
    memory.io.dWriteValue <> io.dWriteValue

    memory.io.address := exMemSignals.stage.execute.result
    memory.io.writeData := exMemSignals.stage.regRead.rs2Value
    memory.io.writeSize := exMemSignals.stage.decode.memWrite
    memory.io.readSize := exMemSignals.stage.decode.memRead

    memorySignalsWire.readData := memory.io.readData

    memWbSignals.pc := exMemSignals.pc
    memWbSignals.stage.fetch := exMemSignals.stage.fetch
    memWbSignals.stage.decode := exMemSignals.stage.decode
    memWbSignals.stage.regRead := exMemSignals.stage.regRead
    memWbSignals.stage.execute := exMemSignals.stage.execute
    memWbSignals.stage.memory := memorySignalsWire

    printf(cf"memoryInst: ${exMemSignals.stage.fetch.instruction.bits}%x memorySignals: $memorySignalsWire\n")
  }

  // write-back
  {
    val writeBack = Module(new WriteBack())

    writeBack.io.memToReg := memWbSignals.stage.decode.memToReg
    writeBack.io.execResult := memWbSignals.stage.execute.result
    writeBack.io.readData := memWbSignals.stage.memory.readData

    writeBackSignalsWire.result := writeBack.io.result

    phyRegs.io.rd := memWbSignals.stage.decode.rd
    phyRegs.io.rdEn := memWbSignals.stage.decode.regWrite
    phyRegs.io.rdValue := writeBack.io.result

    printf(cf"wbInst: ${memWbSignals.stage.fetch.instruction.bits}%x wb: ${writeBack.io.result}%x\n")
  }

  // control
  // Always, check the signals that execute is working on
  // OR the memory stage is working on
  when(rrExSignals.stage.decode.jump) {
    printf(cf"jumping from $pc -> ${executeSignalsWire.nextPc}\n")

    // pc could be stored in executeSignalsWire
    pc := executeSignalsWire.nextPc

    killFetchSignals(ifIdSignals.stage.fetch)
    killDecodeSignals(idRrSignals.stage.decode)
    killDecodeSignals(rrExSignals.stage.decode)
  }.elsewhen(executeSignalsWire.zero && rrExSignals.stage.decode.branch) {
    printf(cf"branching from $pc -> ${executeSignalsWire.nextPc}\n")

    pc := executeSignalsWire.nextPc

    killFetchSignals(ifIdSignals.stage.fetch)
    killDecodeSignals(idRrSignals.stage.decode)
    killDecodeSignals(rrExSignals.stage.decode)
  }.elsewhen(fetchSignalsWire.instruction.valid && !stall) {
    pc := pc + 4.U
  }
}

object InOrderPipelinedCPU {
  // The magic NOP
  // Decode to find why this is the NOP

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
                      writeBackSignalsWire: WriteBackSignals,
                      regReadSignalsDecode: DecodeSignals,
                      executeSignalsDecode: DecodeSignals,
                      memorySignalsDecode: DecodeSignals,
                      executeSignals: ExecuteSignals,
                      out: UInt, stallOut: Bool): Unit = {

    // if any of the future stages have computed the values of
    // the source register, then forward it
    when(0.U === rs) {
      printf(cf"rs = 0\n")

      out := 0.U
      stallOut := false.B
    }.elsewhen(regReadSignalsDecode.regWrite && regReadSignalsDecode.rd === rs) {
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
    }.elsewhen(executeSignalsDecode.regWrite && executeSignalsDecode.rd === rs) {
      // if the executeSignals is holding the value
      // that means the memory stage is working on the instruction

      printf(cf"rs: forwarding memory stage's results: ${executeSignalsDecode.memToReg} ? (memory results) / (execute results)\n")

      // this code is duplicate of the write-back stage
      out := Mux(executeSignalsDecode.memToReg, memorySignalsWire.readData, executeSignals.result)
      stallOut := false.B
    }.elsewhen(memorySignalsDecode.regWrite && memorySignalsDecode.rd === rs) {
      // if memorySignals is holding the value
      // that means the write-back stage is working on the instruction

      out := writeBackSignalsWire.result
      stallOut := false.B
    }.otherwise {
      printf(cf"rs: reading the register\n")

      out := rsValue
      stallOut := false.B
    }
  }
}
