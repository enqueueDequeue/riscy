package io.riscy.stages.signals

import chisel3.util.Valid
import chisel3.{Data, DontCare, fromBooleanToLiteral, fromIntToLiteral}
import io.riscy.stages.{ExecuteOp, MemRWSize}

object Utils {
  val NOP = 0x33
  val PC_INIT = 0

  def initStage[T <: Data](stage: Valid[Stage[T]]) = {
    stage.valid := false.B
    stage.bits.pc := PC_INIT.U
  }

  def initStage[T <: Data](stage: Stage[T]) = {
    stage.pc := PC_INIT.U
  }

  def initValid[T <: Data](v: Valid[T]) = {
    v.valid := false.B
    v.bits := DontCare
  }

  def initFetchSignals(fetchSignals: FetchSignals) = {
    fetchSignals.instruction.valid := false.B
    fetchSignals.instruction.bits := NOP.U
  }

  def initDecodeSignals(decodeSignals: DecodeSignals) = {
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
    decodeSignals.word := false.B
  }

  def initRenameSignals(renameSignals: RenameSignals) = {
    renameSignals.rs1PhyReg := 0.U
    renameSignals.rs2PhyReg := 0.U
    renameSignals.rdPhyReg.valid := false.B
    renameSignals.rdPhyReg.bits := 0.U
  }

  def initRegReadSignals(regReadSignals: RegReadSignals) = {
    regReadSignals.rs1Value := 0.U
    regReadSignals.rs2Value := 0.U
  }

  def initExecuteSignals(executeSignals: ExecuteSignals) = {
    executeSignals.nextPc := PC_INIT.U
    executeSignals.result := 0.U
    executeSignals.zero := false.B
  }

  def initMemorySignals(memorySignals: MemorySignals) = {
    memorySignals.readData := 0.U
  }

  def killFetchSignals(fetchSignals: FetchSignals) = {
    fetchSignals.instruction.valid := false.B
    fetchSignals.instruction.bits := NOP.U
  }

  def killDecodeSignals(decodeSignals: DecodeSignals) = {
    decodeSignals.memRead := MemRWSize.BYTES_NO
    decodeSignals.memWrite := MemRWSize.BYTES_NO
    decodeSignals.regWrite := false.B
    decodeSignals.branch := false.B
    decodeSignals.jump := false.B
    decodeSignals.rd := 0.U
  }
}
