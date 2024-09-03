package io.riscy

import chisel3.util.{Decoupled, Valid, log2Ceil}
import chisel3.{Bundle, DontCare, Flipped, Module, Mux, RegInit, UInt, Wire, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, when}
import io.riscy.stages.signals.Utils.{NOP, PC_INIT, initDecodeSignals, initFetchSignals, initRenameSignals, initRobSignals, initStage}
import io.riscy.stages.signals.{DecodeSignals, ExecuteSignals, FetchSignals, Parameters, ROBSignals, RegReadSignals, RenameSignals, Stage}
import io.riscy.stages.{Decode, Execute, Fetch, InstructionQueue, Memory, MemoryO3, PhyRegs, ROB, Rename}

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

  val ifIdSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initSignals
  })

  val idRenameSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
      val decode = DecodeSignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initDecodeSignals(initSignals.stage.decode)
    initSignals
  })

  val renameRobSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
      val decode = DecodeSignals()
      val rename = RenameSignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initDecodeSignals(initSignals.stage.decode)
    initRenameSignals(initSignals.stage.rename)
    initSignals
  })

  val robIqSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val fetch = FetchSignals()
      val decode = DecodeSignals()
      val rename = RenameSignals()
      val rob = ROBSignals()
    }))
    initStage(initSignals)
    initFetchSignals(initSignals.stage.fetch)
    initDecodeSignals(initSignals.stage.decode)
    initRenameSignals(initSignals.stage.rename)
    initRobSignals(initSignals.stage.rob)
    initSignals
  })

  val iqRRSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val rob = ROBSignals()
    }))
    initStage(initSignals)
    initRobSignals(initSignals.stage.rob)
    initSignals
  })

  val rrExSignals = RegInit({
    val initSignals = Wire(Stage(new Bundle {
      val decode = DecodeSignals()
      val regRead = RegReadSignals()
    }))
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
  val decode = Module(new Decode())

  // decode input
  when(ifIdSignals.stage.fetch.instruction.valid) {
    decode.io.inst := ifIdSignals.stage.fetch.instruction.bits
  }.otherwise {
    decode.io.inst := NOP.U
  }

  // decode output
  idRenameSignals.pc := ifIdSignals.pc
  idRenameSignals.stage.fetch := ifIdSignals.stage.fetch
  idRenameSignals.stage.decode := decode.io.signals

  // Allocations
  // todo: Allocate Phy Reg, ROB, IQ, LDQ/STQ here itself
  //  if allocation fails, then stall

  // Rename
  val rename = Module(new Rename())

  // rename input
  rename.io.rs1 := idRenameSignals.stage.decode.rs1
  rename.io.rs2 := idRenameSignals.stage.decode.rs2
  rename.io.rd.valid := idRenameSignals.stage.decode.regWrite && idRenameSignals.stage.fetch.instruction.valid
  rename.io.rd.bits := idRenameSignals.stage.decode.rd

  // rename output
  renameRobSignals.pc := idRenameSignals.pc
  renameRobSignals.stage.fetch := idRenameSignals.stage.fetch
  renameRobSignals.stage.decode := idRenameSignals.stage.decode
  renameRobSignals.stage.rename.rs1PhyReg := rename.io.rs1PhyReg
  renameRobSignals.stage.rename.rs2PhyReg := rename.io.rs2PhyReg
  renameRobSignals.stage.rename.rdPhyReg := rename.io.rdPhyReg

  // ROB allocate
  val rob = Module(new ROB())

  // rob input
  rob.io.instSignals.valid := renameRobSignals.stage.fetch.instruction.valid
  rob.io.instSignals.bits.pc := renameRobSignals.pc
  rob.io.instSignals.bits.fetchSignals := renameRobSignals.stage.fetch
  rob.io.instSignals.bits.decodeSignals := renameRobSignals.stage.decode
  rob.io.instSignals.bits.renameSignals := renameRobSignals.stage.rename

  // rob output
  robIqSignals.pc := renameRobSignals.pc
  robIqSignals.stage.fetch := renameRobSignals.stage.fetch
  robIqSignals.stage.decode := renameRobSignals.stage.decode
  robIqSignals.stage.rename := renameRobSignals.stage.rename
  robIqSignals.stage.rob.robIdx := rob.io.robIdx

  // Issue
  val iq = Module(new InstructionQueue())

  // iq input
  iq.io.instSignals.valid := robIqSignals.stage.fetch.instruction.valid && robIqSignals.stage.rob.robIdx.valid
  iq.io.instSignals.bits.rs1PhyReg.valid := !robIqSignals.stage.decode.rs1Pc
  iq.io.instSignals.bits.rs2PhyReg.valid := !robIqSignals.stage.decode.rs2Imm
  iq.io.instSignals.bits.rs1PhyReg.bits := robIqSignals.stage.rename.rs1PhyReg
  iq.io.instSignals.bits.rs2PhyReg.bits := robIqSignals.stage.rename.rs2PhyReg
  iq.io.instSignals.bits.robIdx := robIqSignals.stage.rob.robIdx.bits

  // todo: make sure to stall/back propagate
  //  when iq fails to allocate
  // Update back to the rob
  rob.io.iqRobIdx.valid := iq.io.iqIdx.valid
  rob.io.iqRobIdx.bits.iqIdx := iq.io.iqIdx.bits
  rob.io.iqRobIdx.bits.robIdx := robIqSignals.stage.rob.robIdx.bits

  // todo: assign the wakeup regs

  // todo: is this the right pc?
  iqRRSignals.pc := robIqSignals.pc
  iqRRSignals.stage.rob.robIdx.valid := iq.io.readyInstSignals.valid
  iqRRSignals.stage.rob.robIdx.bits := iq.io.readyInstSignals.bits

  // Reg Read
  val registers = Module(new PhyRegs())

  rob.io.readRobIdx := iqRRSignals.stage.rob.robIdx

  registers.io.rs1 := rob.io.robData.bits.decodeSignals.rs1
  registers.io.rs2 := rob.io.robData.bits.decodeSignals.rs2

  // todo: is this the right pc?
  //  or should you use rob signal pc instead?
  val rs1Value = Mux(rob.io.robData.bits.decodeSignals.rs1Pc, iqRRSignals.pc, registers.io.rs1Value)
  val rs2Value = Mux(rob.io.robData.bits.decodeSignals.rs2Imm, rob.io.robData.bits.decodeSignals.immediate, registers.io.rs2Value)

  rrExSignals.pc := iqRRSignals.pc
  rrExSignals.stage.regRead.rs1Value := rs1Value
  rrExSignals.stage.regRead.rs2Value := rs2Value
  rrExSignals.stage.decode := rob.io.robData.bits.decodeSignals

  // Running Execute and Memory stages in parallel

  // Execute
  val execute = Module(new Execute())

  execute.io.a := rrExSignals.stage.regRead.rs1Value
  execute.io.b := rrExSignals.stage.regRead.rs2Value
  execute.io.branchInvert := rrExSignals.stage.decode.branchInvert
  execute.io.word := rrExSignals.stage.decode.word
  execute.io.op := rrExSignals.stage.decode.aluOp

  // Memory
  val memory = Module(new MemoryO3())

  // generate the address

  // Commit

  // branch updates/flushing
}
