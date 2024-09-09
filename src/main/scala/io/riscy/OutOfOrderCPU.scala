package io.riscy

import chisel3.util.{Decoupled, Valid, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Flipped, Module, Mux, RegInit, UInt, Wire, assert, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, when}
import io.riscy.OutOfOrderCPU.isValid
import io.riscy.stages.signals.Utils.{NOP, PC_INIT, initFetchSignals, initStage, initValid}
import io.riscy.stages.signals.{AllocSignals, DecodeSignals, FetchSignals, Parameters, ROBSignals, RegReadSignals, RenameSignals, Stage}
import io.riscy.stages.{Decode, Execute, ExecuteOp, Fetch, InstructionQueue, LoadStoreIndex, MemRWDirection, MemRWSize, MemoryO3, PhyRegs, ROB, Rename}

class OutOfOrderCPU()(implicit val params: Parameters) extends Module {
  val addrWidth = params.addrWidth
  val nROBEntries = params.nROBEntries

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
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
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

  // signals from Execute & Memory to Commit
  val emCommitSignals = RegInit({
    val initSignals = Wire(Valid(Stage(new Bundle {
      val pc = UInt(addrWidth.W)
      val nextPc = UInt(addrWidth.W)
      val zero = Bool()
    })))
    initStage(initSignals)
    initSignals
  })

  // Fetch
  val fetch = Module(new Fetch())

  io.iReadAddr <> fetch.io.iReadAddr
  io.iReadValue <> fetch.io.iReadValue

  // fetch input
  fetch.io.pc := pc

  // fetch output
  when(fetch.io.inst.valid) {
    ifIdSignals.pc := pc
    ifIdSignals.stage.fetch.instruction.valid := true.B
    ifIdSignals.stage.fetch.instruction.bits := fetch.io.inst.deq()
  }.otherwise {
    ifIdSignals.pc := PC_INIT.U
    ifIdSignals.stage.fetch.instruction.valid := false.B
    ifIdSignals.stage.fetch.instruction.bits := DontCare
    fetch.io.inst.nodeq()
  }

  // Decode
  val stall = Wire(Bool())

  val decode = Module(new Decode())
  val rename = Module(new Rename())
  val rob = Module(new ROB())
  val iq = Module(new InstructionQueue())
  val memory = Module(new MemoryO3())

  val shouldAct = ifIdSignals.stage.fetch.instruction.valid

  when(shouldAct) {
    decode.io.inst := ifIdSignals.stage.fetch.instruction.bits

    val decodeSignalsOut = decode.io.signals

    assert(!isValid(decodeSignalsOut.memRead) || !isValid(decodeSignalsOut.memWrite))

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

    // Allocations
    val dstRegW = Wire(Valid(UInt(log2Ceil(params.nPhyRegs).W)))
    val robIdxW = Wire(Valid(UInt(log2Ceil(params.nROBEntries).W)))
    val iqIdxW = Wire(Valid(UInt(log2Ceil(params.nIQEntries).W)))
    val memIdxW = Wire(Valid(new LoadStoreIndex()))

    val regShouldAllocate = decodeSignalsOut.regWrite && decodeSignalsOut.rd =/= 0.U

    when(regShouldAllocate && !dstReg.valid) {
      rename.io.allocate := true.B
      dstRegW := rename.io.allocatedIdx
    }

    when(!robIdx.valid) {
      rob.io.allocate := true.B
      robIdxW := rob.io.allocatedIdx
    }

    when(!iqIdx.valid) {
      iq.io.allocate := true.B
      iqIdxW := iq.io.allocatedIdx
    }

    val memShouldAllocate = isValid(decodeSignalsOut.memRead) || isValid(decodeSignalsOut.memWrite)

    when(memShouldAllocate && memIdx.valid) {
      memory.io.allocate.valid := memShouldAllocate
      memory.io.allocate.bits := Mux(isValid(decodeSignalsOut.memRead), MemRWDirection.read, MemRWDirection.write)

      memIdxW := memory.io.allocatedIdx
    }

    val regAllocSuccess = regShouldAllocate && dstRegW.valid
    val robAllocSuccess = robIdxW.valid
    val iqAllocSuccess = iqIdxW.valid
    val memAllocSuccess = memShouldAllocate && memIdxW.valid

    when(regAllocSuccess && robAllocSuccess && iqAllocSuccess && memAllocSuccess) {
      // proceed
      assert(robIdxW.valid)
      assert(iqIdxW.valid)

      idRenameSignals.valid := true.B
      idRenameSignals.bits.pc := ifIdSignals.pc
      idRenameSignals.bits.stage.fetch := ifIdSignals.stage.fetch
      idRenameSignals.bits.stage.decode := decodeSignalsOut
      idRenameSignals.bits.stage.alloc.dstReg := dstRegW
      idRenameSignals.bits.stage.alloc.robIdx := robIdxW.bits
      idRenameSignals.bits.stage.alloc.iqIdx := iqIdxW.bits
      idRenameSignals.bits.stage.alloc.memIdx := memIdxW

      dstReg.valid := false.B
      robIdxW.valid := false.B
      iqIdx.valid := false.B
      memIdx.valid := false.B
      stall := false.B
    }.otherwise {
      // stall
      idRenameSignals.valid := false.B
      idRenameSignals.bits := DontCare

      dstReg := dstRegW
      robIdx := robIdxW
      iqIdx := iqIdxW
      memIdx := memIdxW
      stall := true.B
    }
  }.otherwise {
    decode.io.inst := NOP.U

    idRenameSignals.valid := false.B
    idRenameSignals.bits.pc := DontCare
    idRenameSignals.bits.stage.fetch := ifIdSignals.stage.fetch
    idRenameSignals.bits.stage.decode := decode.io.signals
    idRenameSignals.bits.stage.alloc := DontCare

    stall := true.B
  }

  // Rename
  // rename input
  rename.io.rs1 := idRenameSignals.bits.stage.decode.rs1
  rename.io.rs2 := idRenameSignals.bits.stage.decode.rs2
  rename.io.rd.valid := idRenameSignals.valid && idRenameSignals.bits.stage.alloc.dstReg.valid && idRenameSignals.bits.stage.decode.regWrite
  rename.io.rd.bits.arch := idRenameSignals.bits.stage.decode.rd
  rename.io.rd.bits.phy := idRenameSignals.bits.stage.alloc.dstReg

  // rename output
  renameRobIqSignals.valid := idRenameSignals.valid
  renameRobIqSignals.bits.pc := idRenameSignals.bits.pc
  renameRobIqSignals.bits.stage.fetch := idRenameSignals.bits.stage.fetch
  renameRobIqSignals.bits.stage.decode := idRenameSignals.bits.stage.decode
  renameRobIqSignals.bits.stage.alloc := idRenameSignals.bits.stage.alloc
  renameRobIqSignals.bits.stage.rename.rs1PhyReg := rename.io.rs1PhyReg
  renameRobIqSignals.bits.stage.rename.rs2PhyReg := rename.io.rs2PhyReg
  renameRobIqSignals.bits.stage.rename.rdPhyReg := idRenameSignals.bits.stage.alloc.dstReg

  // Running ROB and IQ stages in parallel

  // ROB
  // rob input
  rob.io.instSignals.valid := renameRobIqSignals.valid
  rob.io.instSignals.bits.pc := renameRobIqSignals.bits.pc
  rob.io.instSignals.bits.data.robIdx := renameRobIqSignals.bits.stage.alloc.robIdx
  rob.io.instSignals.bits.data.fetchSignals := renameRobIqSignals.bits.stage.fetch
  rob.io.instSignals.bits.data.decodeSignals := renameRobIqSignals.bits.stage.decode
  rob.io.instSignals.bits.data.renameSignals := renameRobIqSignals.bits.stage.rename
  rob.io.instSignals.bits.data.allocSignals := renameRobIqSignals.bits.stage.alloc

  // Issue
  // iq input
  iq.io.instSignals.valid := renameRobIqSignals.valid
  iq.io.instSignals.bits.iqIdx := renameRobIqSignals.bits.stage.alloc.iqIdx
  iq.io.instSignals.bits.robIdx := renameRobIqSignals.bits.stage.alloc.robIdx
  iq.io.instSignals.bits.rs1PhyReg.valid := !(renameRobIqSignals.bits.stage.decode.rs1Pc || renameRobIqSignals.bits.stage.decode.rs1 === 0.U)
  iq.io.instSignals.bits.rs2PhyReg.valid := !(renameRobIqSignals.bits.stage.decode.rs2Imm || renameRobIqSignals.bits.stage.decode.rs2 === 0.U)
  iq.io.instSignals.bits.rs1PhyReg.bits := renameRobIqSignals.bits.stage.rename.rs1PhyReg
  iq.io.instSignals.bits.rs2PhyReg.bits := renameRobIqSignals.bits.stage.rename.rs2PhyReg

  // todo: assign the wakeup regs

  iqRRSignals.valid := iq.io.readyInstSignals.valid
  iqRRSignals.bits.robIdx := iq.io.readyInstSignals.bits

  // Reg Read
  val registers = Module(new PhyRegs())

  rob.io.readRobIdx.valid := iqRRSignals.valid
  rob.io.readRobIdx.bits := iqRRSignals.bits.robIdx

  assert(rob.io.robData.valid === iqRRSignals.valid)

  registers.io.rs1 := rob.io.robData.bits.data.renameSignals.rs1PhyReg
  registers.io.rs2 := rob.io.robData.bits.data.renameSignals.rs2PhyReg

  rrExMemSignals.valid := iqRRSignals.valid
  rrExMemSignals.bits.pc := rob.io.robData.bits.pc
  rrExMemSignals.bits.stage.rob := rob.io.robData.bits
  rrExMemSignals.bits.stage.regRead.rs1Value := registers.io.rs1Value
  rrExMemSignals.bits.stage.regRead.rs2Value := registers.io.rs2Value

  // Running Execute and Memory stages in parallel

  // Execute
  val execute = Module(new Execute())

  when(rrExMemSignals.valid) {
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

    // todo: nextPc need not be calculated as such
    //  The only calculation that needs to be done is:
    //  If the branch is predicted taken or not and if the result is taken or not

    val nextPc = Mux(rrExMemSignals.bits.stage.rob.decodeSignals.jump, execute.io.result, pcImm)
    val result = Mux(rrExMemSignals.bits.stage.rob.decodeSignals.jump, pc4, execute.io.result)

    when(rrExMemSignals.bits.stage.rob.decodeSignals.branch && execute.io.zero) {

    }.elsewhen(rrExMemSignals.bits.stage.rob.decodeSignals.jump) {

    }

    registers.io.rdEn := rrExMemSignals.bits.stage.rob.allocSignals.dstReg.valid
    registers.io.rd := rrExMemSignals.bits.stage.rob.allocSignals.dstReg.bits
    registers.io.rdValue := execute.io.result // todo: pass the right value here
  }.otherwise {
    execute.io.a := 0.U
    execute.io.b := 0.U
    execute.io.branchInvert := false.B
    execute.io.word := false.B
    execute.io.op := ExecuteOp.NOP
  }

  // Memory
  memory.io.dMem <> io.dMem
  memory.io.dMemAck <> io.dMemAck

  // todo: maybe break up the memory stage to
  //  address generation and actual memory?
  val address = rrExMemSignals.bits.stage.regRead.rs1Value + rrExMemSignals.bits.stage.rob.decodeSignals.immediate

  memory.io.readData.valid := isValid(rrExMemSignals.bits.stage.rob.decodeSignals.memRead)
  memory.io.readData.bits.address := address
  memory.io.readData.bits.size := rrExMemSignals.bits.stage.rob.decodeSignals.memRead
  memory.io.readData.bits.robIdx := rrExMemSignals.bits.stage.rob.allocSignals.robIdx
  memory.io.readData.bits.dstReg := rrExMemSignals.bits.stage.rob.allocSignals.dstReg
  memory.io.readData.bits.ldIdx := rrExMemSignals.bits.stage.rob.allocSignals.memIdx.bits.asLoadIndex()

  memory.io.writeData.valid := isValid(rrExMemSignals.bits.stage.rob.decodeSignals.memWrite)
  memory.io.writeData.bits.address := address
  memory.io.writeData.bits.size := rrExMemSignals.bits.stage.rob.decodeSignals.memWrite
  memory.io.writeData.bits.data := rrExMemSignals.bits.stage.regRead.rs2Value
  memory.io.writeData.bits.stIdx := rrExMemSignals.bits.stage.rob.allocSignals.memIdx.bits.asStoreIndex()

  when(memory.io.readDataOut.valid && memory.io.readDataOut.bits.dstReg.valid) {
    registers.io.rd2En := true.B
    registers.io.rd2 := memory.io.readDataOut.bits.dstReg.bits
    registers.io.rd2Value := memory.io.readDataOut.bits.data
  }.otherwise {
    registers.io.rd2En := false.B
    registers.io.rd2 := DontCare
    registers.io.rd2Value := DontCare
  }

  // Commit
  memory.io.commitIdx.valid := rob.io.retireInst.valid && rob.io.retireInst.bits.allocSignals.memIdx.valid
  memory.io.commitIdx.bits := rob.io.retireInst.bits.allocSignals.memIdx.bits

  // todo: compare the flush indices of execute stage
  //  and memory stage, flush the closest value to rob head
  rob.io.flush := memory.io.flushIdx
  // branch updates/flushing
}

object OutOfOrderCPU {
  def isValid(size: MemRWSize.Type): Bool = {
    size =/= MemRWSize.BYTES_NO
  }
}
