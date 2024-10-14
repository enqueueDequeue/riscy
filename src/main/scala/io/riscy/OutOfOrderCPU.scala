package io.riscy

import chisel3.util.{Decoupled, Valid, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Flipped, Module, Mux, PrintableHelper, RegInit, UInt, Vec, Wire, assert, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.OutOfOrderCPU.isValid
import io.riscy.stages.signals.Utils.{NOP, initStage, initValid}
import io.riscy.stages.signals.{AllocSignals, DecodeSignals, FetchSignals, Parameters, ROBSignals, RegReadSignals, RenameSignals, Stage}
import io.riscy.stages.{Decode, Execute, ExecuteOp, Fetch, InstructionQueue, LoadStoreIndex, MemRWDirection, MemRWSize, MemoryO3, PhyRegs, ROB, Rename}

class OutOfOrderCPU()(implicit val params: Parameters) extends Module {
  val addrWidth = params.addrWidth
  val nROBEntries = params.nROBEntries
  val nPhyRegs = params.nPhyRegs

  val pc = RegInit(0.U(addrWidth.W))

  val io = IO(new Bundle {
    val iReadAddr = Decoupled(UInt(params.addrWidth.W))
    val iReadValue = Flipped(Decoupled(UInt(params.instWidth.W)))

    val dMem = Decoupled(new Bundle {
      val addr = UInt(addrWidth.W)
      val size = UInt(log2Ceil(params.dataWidth / params.bitWidth).W)

      // If write is valid
      // Then operation is treated as write
      // Else
      // It's treated as read
      val write = Valid(new Bundle {
        val mask = UInt((params.dataWidth / params.bitWidth).W)
        val value = UInt(params.dataWidth.W)
      })
    })

    val dMemAck = Flipped(Decoupled(new Bundle {
      val size = UInt(log2Ceil(params.dataWidth / params.bitWidth).W)
      val value = UInt(params.dataWidth.W)
    }))
  })

  // signals from fetch to decode
  val ifIdSignals = RegInit({
    val initSignals = Wire(Valid(Stage(new Bundle {
      val fetch = FetchSignals()
    })))
    initStage(initSignals)
    initSignals
  })

  // signals from decode to rename
  val idRenameSignals = RegInit({
    val initSignals = Wire(Valid(Stage(new Bundle {
      val fetch = FetchSignals()
      val decode = DecodeSignals()
      val alloc = AllocSignals()
    })))
    initStage(initSignals)
    initSignals
  })

  // signals from rename to ROB & IQ
  val renameRobIqSignals = RegInit({
    val initSignals = Wire(Valid(Stage(new Bundle {
      val fetch = FetchSignals()
      val decode = DecodeSignals()
      val alloc = AllocSignals()
      val rename = RenameSignals()
    })))
    initStage(initSignals)
    initSignals
  })

  // signals from IQ to Register Read
  val iqRRSignals = RegInit({
    val initSignals = Wire(Valid(new Bundle {
      val robIdx = UInt(log2Ceil(nROBEntries).W)
    }))
    initValid(initSignals)
    initSignals
  })

  // signals from Register Read to Execute & Memory
  val rrExMemSignals = RegInit({
    val initSignals = Wire(Valid(Stage(new Bundle {
      val regRead = RegReadSignals()
      val rob = new ROBSignals()
    })))
    initStage(initSignals)
    initSignals
  })

  val wakeupRegs = RegInit(0.U(nPhyRegs.W))
  val wakeupRegsW = Wire(Vec(nPhyRegs, Bool()))

  wakeupRegsW := wakeupRegs.asBools

  val flush = Wire(Bool())
  val fStall = Wire(Bool())
  val dStall = Wire(Bool())

  // Fetch
  val fetch = Module(new Fetch())

  io.iReadAddr <> fetch.io.iReadAddr
  io.iReadValue <> fetch.io.iReadValue

  // fetch input
  fetch.io.pc := pc

  val ifIdSignalsW = Wire(Valid(Stage(new Bundle {
    val fetch = FetchSignals()
  })))

  // fetch output
  when(fetch.io.inst.valid) {
    ifIdSignalsW.valid := true.B
    ifIdSignalsW.bits.pc := pc
    ifIdSignalsW.bits.stage.fetch.instruction.valid := true.B
    ifIdSignalsW.bits.stage.fetch.instruction.bits := fetch.io.inst.bits

    fStall := false.B
  }.otherwise {
    ifIdSignalsW.valid := false.B
    ifIdSignalsW.bits := DontCare

    fStall := true.B
  }

  // don't update the
  // signals on dStall
  when(!dStall) {
    ifIdSignals := ifIdSignalsW
  }

  when(flush) {
    fStall := false.B
    ifIdSignals.valid := false.B
  }

  // Decode
  val decode = Module(new Decode())
  val rename = Module(new Rename())
  val rob = Module(new ROB())
  val iq = Module(new InstructionQueue())
  val memory = Module(new MemoryO3())

  // NOTE: In case the decode stage requested for a stall in the
  // previous cycle, the ifIdSignals will remain same in this cycle
  // So, decode should result in the same result.

  val dstReg = RegInit({
    val initSignals = Wire(Valid(UInt(log2Ceil(params.nPhyRegs).W)))

    initSignals.valid := false.B
    initSignals.bits := DontCare

    initSignals
  })
  val robIdx = RegInit({
    val initSignals = Wire(Valid(UInt(log2Ceil(nROBEntries).W)))

    initSignals.valid := false.B
    initSignals.bits := DontCare

    initSignals
  })
  val iqIdx = RegInit({
    val initSignals = Wire(Valid(UInt(log2Ceil(params.nIQEntries).W)))

    initSignals.valid := false.B
    initSignals.bits := DontCare

    initSignals
  })
  val memIdx = RegInit({
    val initSignals = Wire(Valid(new LoadStoreIndex()))

    initSignals.valid := false.B
    initSignals.bits := DontCare

    initSignals
  })

  when(flush) {
    dstReg.valid := false.B
    robIdx.valid := false.B
    iqIdx.valid := false.B
    memIdx.valid := false.B
  }

  when(flush || !ifIdSignals.valid || !ifIdSignals.bits.stage.fetch.instruction.valid) {
    rename.io.allocate := false.B
    rob.io.allocate := false.B
    iq.io.allocate := false.B
    memory.io.allocate.valid := false.B
    memory.io.allocate.bits := DontCare

    decode.io.inst := NOP.U

    idRenameSignals.valid := false.B
    idRenameSignals.bits.pc := DontCare
    idRenameSignals.bits.stage.fetch := ifIdSignals.bits.stage.fetch
    idRenameSignals.bits.stage.decode := decode.io.signals
    idRenameSignals.bits.stage.alloc := DontCare

    dStall := false.B
  }.otherwise {
    decode.io.inst := ifIdSignals.bits.stage.fetch.instruction.bits

    val decodeSignalsOut = decode.io.signals

    assert(!isValid(decodeSignalsOut.memRead) || !isValid(decodeSignalsOut.memWrite))

    // Allocations
    val dstRegW = Wire(Valid(UInt(log2Ceil(params.nPhyRegs).W)))
    val robIdxW = Wire(Valid(UInt(log2Ceil(params.nROBEntries).W)))
    val iqIdxW = Wire(Valid(UInt(log2Ceil(params.nIQEntries).W)))
    val memIdxW = Wire(Valid(new LoadStoreIndex()))

    val regShouldAllocate = decodeSignalsOut.regWrite && decodeSignalsOut.rd =/= 0.U

    when(regShouldAllocate && !dstReg.valid) {
      rename.io.allocate := true.B
      dstRegW := rename.io.allocatedIdx
    }.otherwise {
      rename.io.allocate := false.B
      dstRegW := dstReg
    }

    when(!robIdx.valid) {
      rob.io.allocate := true.B
      robIdxW := rob.io.allocatedIdx
    }.otherwise {
      rob.io.allocate := false.B
      robIdxW := robIdx
    }

    when(!iqIdx.valid) {
      iq.io.allocate := true.B
      iqIdxW := iq.io.allocatedIdx
    }.otherwise {
      iq.io.allocate := false.B
      iqIdxW := iqIdx
    }

    val memShouldAllocate = isValid(decodeSignalsOut.memRead) || isValid(decodeSignalsOut.memWrite)

    when(memShouldAllocate && !memIdx.valid) {
      memory.io.allocate.valid := memShouldAllocate
      memory.io.allocate.bits := Mux(isValid(decodeSignalsOut.memRead), MemRWDirection.read, MemRWDirection.write)

      memIdxW := memory.io.allocatedIdx
    }.otherwise {
      memory.io.allocate.valid := false.B
      memory.io.allocate.bits := DontCare

      memIdxW := memIdx
    }

    val regAllocSuccess = (regShouldAllocate && dstRegW.valid) || (!regShouldAllocate)
    val robAllocSuccess = robIdxW.valid
    val iqAllocSuccess = iqIdxW.valid
    val memAllocSuccess = (memShouldAllocate && memIdxW.valid) || (!memShouldAllocate)

    printf(cf"O3: decodedSignals: $decodeSignalsOut\n")
    printf(cf"O3: regShouldAllocate:  $regShouldAllocate  ~ $dstRegW\n")
    printf(cf"O3: robAllocSuccess:    $robAllocSuccess    ~ $robIdxW\n")
    printf(cf"O3: iqAllocSuccess:     $iqAllocSuccess     ~ $iqIdxW\n")
    printf(cf"O3: memShouldAllocate:  $memShouldAllocate  ~ $memIdxW\n")

    when(regAllocSuccess && robAllocSuccess && iqAllocSuccess && memAllocSuccess) {
      // proceed
      assert(robIdxW.valid)
      assert(iqIdxW.valid)

      idRenameSignals.valid := true.B
      idRenameSignals.bits.pc := ifIdSignals.bits.pc
      idRenameSignals.bits.stage.fetch := ifIdSignals.bits.stage.fetch
      idRenameSignals.bits.stage.decode := decodeSignalsOut
      idRenameSignals.bits.stage.alloc.dstReg := dstRegW
      idRenameSignals.bits.stage.alloc.robIdx := robIdxW.bits
      idRenameSignals.bits.stage.alloc.iqIdx := iqIdxW.bits
      idRenameSignals.bits.stage.alloc.memIdx := memIdxW

      dstReg.valid := false.B
      robIdx.valid := false.B
      iqIdx.valid := false.B
      memIdx.valid := false.B

      dStall := false.B
    }.otherwise {
      // stall
      idRenameSignals.valid := false.B
      idRenameSignals.bits := DontCare

      dstReg := dstRegW
      robIdx := robIdxW
      iqIdx := iqIdxW
      memIdx := memIdxW
      dStall := true.B
    }
  }

  // Rename
  // rename input
  rename.io.rs1 := idRenameSignals.bits.stage.decode.rs1
  rename.io.rs2 := idRenameSignals.bits.stage.decode.rs2
  rename.io.rd.valid := !flush && idRenameSignals.valid && idRenameSignals.bits.stage.alloc.dstReg.valid && idRenameSignals.bits.stage.decode.regWrite
  rename.io.rd.bits.arch := idRenameSignals.bits.stage.decode.rd
  rename.io.rd.bits.phy := idRenameSignals.bits.stage.alloc.dstReg.bits

  when(rename.io.deallocationIdx.valid) {
    wakeupRegsW(rename.io.deallocationIdx.bits) := false.B
  }

  // rename output
  renameRobIqSignals.valid := idRenameSignals.valid
  renameRobIqSignals.bits.pc := idRenameSignals.bits.pc
  renameRobIqSignals.bits.stage.fetch := idRenameSignals.bits.stage.fetch
  renameRobIqSignals.bits.stage.decode := idRenameSignals.bits.stage.decode
  renameRobIqSignals.bits.stage.alloc := idRenameSignals.bits.stage.alloc
  renameRobIqSignals.bits.stage.rename.rs1PhyReg := rename.io.rs1PhyReg
  renameRobIqSignals.bits.stage.rename.rs2PhyReg := rename.io.rs2PhyReg
  renameRobIqSignals.bits.stage.rename.rdPhyReg := idRenameSignals.bits.stage.alloc.dstReg

  when(flush) {
    renameRobIqSignals.valid := false.B
  }

  // Running ROB and IQ stages in parallel

  // ROB
  // rob input
  rob.io.instSignals.valid := !flush && renameRobIqSignals.valid
  rob.io.instSignals.bits.pc := renameRobIqSignals.bits.pc
  rob.io.instSignals.bits.data.robIdx := renameRobIqSignals.bits.stage.alloc.robIdx
  rob.io.instSignals.bits.data.fetchSignals := renameRobIqSignals.bits.stage.fetch
  rob.io.instSignals.bits.data.decodeSignals := renameRobIqSignals.bits.stage.decode
  rob.io.instSignals.bits.data.renameSignals := renameRobIqSignals.bits.stage.rename
  rob.io.instSignals.bits.data.allocSignals := renameRobIqSignals.bits.stage.alloc

  // Issue
  // iq input
  iq.io.instSignals.valid := !flush && renameRobIqSignals.valid
  iq.io.instSignals.bits.iqIdx := renameRobIqSignals.bits.stage.alloc.iqIdx
  iq.io.instSignals.bits.robIdx := renameRobIqSignals.bits.stage.alloc.robIdx

  // Stores are the only kinds of instructions which both specify rs2Imm and still use rs2
  // This logic could be useful to breakup the loads and stores into 2 uops 1 address generation and 1 memory
  iq.io.instSignals.bits.rs1PhyReg.valid := !(renameRobIqSignals.bits.stage.decode.rs1Pc || renameRobIqSignals.bits.stage.decode.rs1 === 0.U)
  iq.io.instSignals.bits.rs2PhyReg.valid := isValid(renameRobIqSignals.bits.stage.decode.memWrite) || !(renameRobIqSignals.bits.stage.decode.rs2Imm || renameRobIqSignals.bits.stage.decode.rs2 === 0.U)

  iq.io.instSignals.bits.rs1PhyReg.bits := renameRobIqSignals.bits.stage.rename.rs1PhyReg
  iq.io.instSignals.bits.rs2PhyReg.bits := renameRobIqSignals.bits.stage.rename.rs2PhyReg

  iq.io.wakeUpRegs := wakeupRegsW.asUInt

  iqRRSignals.valid := iq.io.readyInstSignals.valid
  iqRRSignals.bits.robIdx := iq.io.readyInstSignals.bits

  when(flush) {
    iqRRSignals.valid := false.B
  }

  // Reg Read
  val registers = Module(new PhyRegs())

  rob.io.readRobIdx.valid := !flush && iqRRSignals.valid
  rob.io.readRobIdx.bits := iqRRSignals.bits.robIdx

  when(rob.io.robData.valid) {
    printf(cf"O3: work began on: ${rob.io.robData}\n")
  }.otherwise {
    printf(cf"O3: pipeline stalled\n")
  }

  registers.io.rs1 := rob.io.robData.bits.data.renameSignals.rs1PhyReg
  registers.io.rs2 := rob.io.robData.bits.data.renameSignals.rs2PhyReg

  rrExMemSignals.valid := iqRRSignals.valid
  rrExMemSignals.bits.pc := rob.io.robData.bits.pc
  rrExMemSignals.bits.stage.rob := rob.io.robData.bits.data
  rrExMemSignals.bits.stage.regRead.rs1Value := Mux(rob.io.robData.bits.data.decodeSignals.rs1 === 0.U, 0.U, registers.io.rs1Value)
  rrExMemSignals.bits.stage.regRead.rs2Value := Mux(rob.io.robData.bits.data.decodeSignals.rs2 === 0.U, 0.U, registers.io.rs2Value)

  when(flush) {
    rrExMemSignals.valid := false.B
  }

  // Running Execute and Memory stages in parallel
  printf(cf"O3: rs1: ${rrExMemSignals.bits.stage.rob.renameSignals.rs1PhyReg} = ${rrExMemSignals.bits.stage.regRead.rs1Value}\n")
  printf(cf"O3: rs2: ${rrExMemSignals.bits.stage.rob.renameSignals.rs2PhyReg} = ${rrExMemSignals.bits.stage.regRead.rs2Value}\n")
  printf(cf"O3: immediate: ${rrExMemSignals.bits.stage.rob.decodeSignals.immediate}\n")

  // Execute
  val execute = Module(new Execute())

  val exFlushIdx = Wire(Valid(new Bundle {
    val nextPc = UInt(addrWidth.W)
    val robIdx = UInt(log2Ceil(nROBEntries).W)
  }))

  when(!flush && rrExMemSignals.valid
       && !isValid(rrExMemSignals.bits.stage.rob.decodeSignals.memRead)
       && !isValid(rrExMemSignals.bits.stage.rob.decodeSignals.memWrite)) {

    val rs1ValueIntermediate = Mux(rrExMemSignals.bits.stage.rob.decodeSignals.rs1 === 0.U, 0.U, rrExMemSignals.bits.stage.regRead.rs1Value)
    val rs2ValueIntermediate = Mux(rrExMemSignals.bits.stage.rob.decodeSignals.rs2 === 0.U, 0.U, rrExMemSignals.bits.stage.regRead.rs2Value)

    val rs1Value = Mux(rrExMemSignals.bits.stage.rob.decodeSignals.rs1Pc, rrExMemSignals.bits.pc, rs1ValueIntermediate)
    val rs2Value = Mux(rrExMemSignals.bits.stage.rob.decodeSignals.rs2Imm, rrExMemSignals.bits.stage.rob.decodeSignals.immediate, rs2ValueIntermediate)

    execute.io.a := rs1Value
    execute.io.b := rs2Value
    execute.io.branchInvert := rrExMemSignals.bits.stage.rob.decodeSignals.branchInvert
    execute.io.word := rrExMemSignals.bits.stage.rob.decodeSignals.word
    execute.io.op := rrExMemSignals.bits.stage.rob.decodeSignals.aluOp

    val pc4 = rrExMemSignals.bits.pc + 4.U
    val pcImm = rrExMemSignals.bits.pc + rrExMemSignals.bits.stage.rob.decodeSignals.immediate

    // In case of JALR,   result = pc + 4, pc = [rs1] + imm
    // In case of JAL,    result = pc + 4, pc = pc + imm
    // In case of branch  result = <res>,  pc = if (zero) pc + imm else pc + 4

    rob.io.predictionRobIdx.valid := true.B
    rob.io.predictionRobIdx.bits := rrExMemSignals.bits.stage.rob.allocSignals.robIdx

    val predictedNextPcValid = rob.io.prediction.valid
    val predictedNextPc = rob.io.prediction.bits.pc
    val nextRobIdx = rob.io.prediction.bits.robIdx

    when(rrExMemSignals.bits.stage.rob.decodeSignals.branch) {
      val actualNextPc = Mux(execute.io.zero, pcImm, pc4)

      exFlushIdx.valid := (predictedNextPcValid && predictedNextPc =/= actualNextPc) || (!predictedNextPcValid)
      exFlushIdx.bits.nextPc := actualNextPc
      exFlushIdx.bits.robIdx := nextRobIdx
    }.elsewhen(rrExMemSignals.bits.stage.rob.decodeSignals.jump) {
      val actualNextPc = execute.io.result

      exFlushIdx.valid := (predictedNextPcValid && predictedNextPc =/= actualNextPc) || (!predictedNextPcValid)
      exFlushIdx.bits.nextPc := actualNextPc
      exFlushIdx.bits.robIdx := nextRobIdx
    }.otherwise {
      exFlushIdx.valid := false.B
      exFlushIdx.bits := DontCare
    }

    // NOTE: Jump is the only special instruction which
    //       performs two operations: store pc+4 to the dst register
    //       and update the PC (in this case mark the flush if mis-predicted)

    // actual result of the execute stage
    val result = Mux(rrExMemSignals.bits.stage.rob.decodeSignals.jump, pc4, execute.io.result)

    // if there's not need to write to the register, we wouldn't have allocated in
    // the first place. So, rdEn = dstReg.valid
    when(rrExMemSignals.bits.stage.rob.allocSignals.dstReg.valid && !rrExMemSignals.bits.stage.rob.decodeSignals.memToReg) {
      val dstReg = rrExMemSignals.bits.stage.rob.allocSignals.dstReg.bits

      wakeupRegsW(dstReg) := true.B

      registers.io.rdEn := true.B
      registers.io.rd := dstReg
      registers.io.rdValue := result

      printf(cf"O3: rd: robIdx -> ${rrExMemSignals.bits.stage.rob.robIdx} dst: $dstReg = $result\n")
    }.otherwise {
      registers.io.rdEn := false.B
      registers.io.rd := DontCare
      registers.io.rdValue := DontCare
    }

    rob.io.commitRobIdx0.valid := true.B
    rob.io.commitRobIdx0.bits := rrExMemSignals.bits.stage.rob.robIdx
  }.otherwise {
    execute.io.a := 0.U
    execute.io.b := 0.U
    execute.io.branchInvert := false.B
    execute.io.word := false.B
    execute.io.op := ExecuteOp.NOP

    exFlushIdx.valid := false.B
    exFlushIdx.bits := DontCare

    registers.io.rdEn := false.B
    registers.io.rd := DontCare
    registers.io.rdValue := DontCare

    rob.io.predictionRobIdx.valid := false.B
    rob.io.predictionRobIdx.bits := DontCare

    rob.io.commitRobIdx0.valid := false.B
    rob.io.commitRobIdx0.bits := DontCare
  }

  // Memory

  // NOTE: Using execute port to sneakily commit memory data.
  //       A store upon reaching the Memory stage is deemed to be commited.
  when(rrExMemSignals.valid && isValid(rrExMemSignals.bits.stage.rob.decodeSignals.memWrite)) {
    rob.io.commitRobIdx0.valid := true.B
    rob.io.commitRobIdx0.bits := rrExMemSignals.bits.stage.rob.allocSignals.robIdx
  }

  memory.io.dMem <> io.dMem
  memory.io.dMemAck <> io.dMemAck

  val address = rrExMemSignals.bits.stage.regRead.rs1Value + rrExMemSignals.bits.stage.rob.decodeSignals.immediate

  memory.io.readData.valid := !flush && rrExMemSignals.valid && isValid(rrExMemSignals.bits.stage.rob.decodeSignals.memRead)
  memory.io.readData.bits.address := address
  memory.io.readData.bits.size := rrExMemSignals.bits.stage.rob.decodeSignals.memRead
  memory.io.readData.bits.robIdx := rrExMemSignals.bits.stage.rob.allocSignals.robIdx
  memory.io.readData.bits.dstReg := rrExMemSignals.bits.stage.rob.allocSignals.dstReg
  memory.io.readData.bits.ldIdx := rrExMemSignals.bits.stage.rob.allocSignals.memIdx.bits.asLoadIndex()

  // currently only throwing exceptions from memory
  // actually, there can be exceptions from anywhere
  // Like:
  // divide by zero in ALU
  // OR
  // accessing a page that is not supposed to be
  // So, technically, the actual logic should be just like
  // the logic of flush. The first exception (closest to the rob head)
  // is the only one that needs to be propagated anyway.
  rob.io.exception.valid := memory.io.readException.valid
  rob.io.exception.bits.robIdx := memory.io.readException.bits

  memory.io.writeData.valid := !flush && rrExMemSignals.valid && isValid(rrExMemSignals.bits.stage.rob.decodeSignals.memWrite)
  memory.io.writeData.bits.address := address
  memory.io.writeData.bits.size := rrExMemSignals.bits.stage.rob.decodeSignals.memWrite
  memory.io.writeData.bits.data := rrExMemSignals.bits.stage.regRead.rs2Value
  memory.io.writeData.bits.stIdx := rrExMemSignals.bits.stage.rob.allocSignals.memIdx.bits.asStoreIndex()

  when(memory.io.readDataOut.valid && memory.io.readDataOut.bits.dstReg.valid) {
    wakeupRegsW(memory.io.readDataOut.bits.dstReg.bits) := true.B

    registers.io.rd2En := true.B
    registers.io.rd2 := memory.io.readDataOut.bits.dstReg.bits
    registers.io.rd2Value := memory.io.readDataOut.bits.data

    printf(cf"O3: rd2: ${memory.io.readDataOut.bits.dstReg.bits} = ${memory.io.readDataOut.bits.data}\n")
  }.otherwise {
    registers.io.rd2En := false.B
    registers.io.rd2 := DontCare
    registers.io.rd2Value := DontCare
  }

  rob.io.commitRobIdx1.valid := memory.io.readDataOut.valid
  rob.io.commitRobIdx1.bits := memory.io.readDataOut.bits.robIdx

  // Commit
  memory.io.retireIdx.valid := rob.io.retireInst.valid && !rob.io.retireInst.bits.flush && rob.io.retireInst.bits.signals.allocSignals.memIdx.valid
  memory.io.retireIdx.bits := rob.io.retireInst.bits.signals.allocSignals.memIdx.bits

  val memFlushIdx = memory.io.flushIdx

  printf(cf"O3: exFlushIdx: $exFlushIdx\n")
  printf(cf"O3: memFlushIdx: $memFlushIdx\n")

  when(!exFlushIdx.valid && !memFlushIdx.valid) {
    rob.io.flush.valid := false.B
    rob.io.flush.bits := DontCare
  }.elsewhen(exFlushIdx.valid && memFlushIdx.valid) {
    val flushEx = dist(exFlushIdx.bits.robIdx) < dist(memFlushIdx.bits)

    rob.io.flush.valid := true.B
    rob.io.flush.bits.robIdx := Mux(flushEx, exFlushIdx.bits.robIdx, memFlushIdx.bits)
    rob.io.flush.bits.updatePc.valid := flushEx
    rob.io.flush.bits.updatePc.bits := exFlushIdx.bits.nextPc
  }.elsewhen(exFlushIdx.valid) {
    rob.io.flush.valid := true.B
    rob.io.flush.bits.robIdx := exFlushIdx.bits.robIdx
    rob.io.flush.bits.updatePc.valid := true.B
    rob.io.flush.bits.updatePc.bits := exFlushIdx.bits.nextPc
  }.elsewhen(memFlushIdx.valid) {
    rob.io.flush.valid := true.B
    rob.io.flush.bits.robIdx := memFlushIdx.bits
    rob.io.flush.bits.updatePc.valid := false.B
    rob.io.flush.bits.updatePc.bits := DontCare
  }.otherwise {
    assert(false.B, "Impossible state detected, dev error!")

    rob.io.flush.valid := false.B
    rob.io.flush.bits := DontCare
  }

  // branch updates/flushing
  when(rob.io.retireInst.valid) {
    when(rob.io.retireInst.bits.flush) {

      flush := true.B
      iq.io.flush := true.B
      memory.io.flush := true.B
      rename.io.flush := true.B
      rename.io.retire.valid := false.B
      rename.io.retire.bits := DontCare
    }.otherwise {
      // just commit

      flush := false.B
      iq.io.flush := false.B
      memory.io.flush := false.B
      rename.io.flush := false.B
      rename.io.retire.valid := rob.io.retireInst.bits.signals.allocSignals.dstReg.valid
      rename.io.retire.bits.arch := rob.io.retireInst.bits.signals.decodeSignals.rd
      rename.io.retire.bits.phy := rob.io.retireInst.bits.signals.allocSignals.dstReg.bits
    }
  }.otherwise {
    flush := false.B

    iq.io.flush := false.B
    memory.io.flush := false.B
    rename.io.flush := false.B
    rename.io.retire.valid := false.B
    rename.io.retire.bits := DontCare
  }

  printf(cf"O3: fStall: $fStall, dStall: $dStall, flush: $flush\n")

  val stall = fStall || dStall

  // no flush or stall, so go ahead with execution
  when(!flush && !stall) {
    pc := pc + 4.U
  }

  when(flush) {
    assert(rename.io.allocatedRegs.valid, "Logic error in rename")

    pc := rob.io.retireInst.bits.pc
    wakeupRegsW := rename.io.allocatedRegs.bits.asBools
  }

  wakeupRegs := wakeupRegsW.asUInt

  // lower value implies older instruction
  def dist(robIdx: UInt): UInt = {
    robIdx - rob.io.robHead
  }
}

object OutOfOrderCPU {
  def isValid(size: MemRWSize.Type): Bool = {
    size =/= MemRWSize.BYTES_NO
  }
}
