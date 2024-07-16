package io.riscy.stages

import chisel3.util.{Valid, isPow2, log2Ceil}
import chisel3.{Bundle, DontCare, Input, Module, Output, PrintableHelper, RegInit, UInt, VecInit, Wire, assert, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.signals.Parameters

class Rename()(implicit val params: Parameters) extends Module {
  val nPhyRegs = params.nPhyRegs
  val nArchRegs = params.nArchRegs

  require(isPow2(nPhyRegs))

  val io = IO(new Bundle {
    val rs1 = Input(UInt(log2Ceil(nArchRegs).W))
    val rs2 = Input(UInt(log2Ceil(nArchRegs).W))
    val rd = Input(UInt(log2Ceil(nArchRegs).W))

    val rs1PhyReg = Output(UInt(log2Ceil(nPhyRegs).W))
    val rs2PhyReg = Output(UInt(log2Ceil(nPhyRegs).W))
    val rdPhyReg = Output(Valid(UInt(log2Ceil(nPhyRegs).W)))

    val commit = Input(Valid(new Bundle {
      val archReg = UInt(log2Ceil(nArchRegs).W)
      val phyReg = UInt(log2Ceil(nPhyRegs).W)
    }))
  })

  // A good reference:
  // https://cs.stackexchange.com/questions/103433/when-to-free-physical-registers

  // right after reset
  // freeRegIdx = 0 => PhyReg1 is free
  // adding 1 to avoid overflow
  val freeRegIdx = RegInit(0.U(log2Ceil(nPhyRegs + 1).W))
  val freeRegs = RegInit(VecInit(Seq.tabulate(nPhyRegs) { _.U(nPhyRegs.W) }))

  // register alias tables
  val rat = RegInit(VecInit(Seq.fill(nArchRegs)(0.U(log2Ceil(nPhyRegs).W))))
  val retirementRat = RegInit(VecInit(Seq.fill(nArchRegs) {
    val initSignals = Wire(Valid(UInt(log2Ceil(nPhyRegs).W)))

    initSignals.valid := false.B
    initSignals.bits := 0.U

    initSignals
  }))

  // helpers along the way
  val actFreeRegIdx = freeRegIdx(log2Ceil(nPhyRegs) - 1, 0)

  assert(actFreeRegIdx === freeRegIdx || freeRegIdx === nPhyRegs.U, "rename integrity failure")

  io.rs1PhyReg := rat(io.rs1)
  io.rs2PhyReg := rat(io.rs2)

  when(io.commit.valid) {
    val freeRegister = retirementRat(io.commit.bits.archReg)

    // mark the current register as free if it's valid
    printf(cf"freeing: $freeRegister, commiting: p${io.commit.bits.phyReg} @ a${io.commit.bits.archReg}\n")

    when(freeRegister.valid) {
      assert(freeRegIdx =/= 0.U, "freeRegIdx cannot be 0 when commiting something")

      val nextFreeRegIdx = actFreeRegIdx - 1.U

      freeRegs(nextFreeRegIdx) := freeRegister.bits
      freeRegIdx := nextFreeRegIdx
    }

    retirementRat(io.commit.bits.archReg).valid := true.B
    retirementRat(io.commit.bits.archReg).bits := io.commit.bits.phyReg
  }

  when(freeRegIdx < nPhyRegs.U) {
    val phyReg = freeRegs(actFreeRegIdx)

    printf(cf"Allocating p$phyReg <- a${io.rd} @ $freeRegIdx\n")

    io.rdPhyReg.valid := true.B
    io.rdPhyReg.bits := phyReg

    rat(io.rd) := phyReg

    freeRegIdx := freeRegIdx + 1.U
  }.otherwise {
    printf(cf"Cannot allocate for a${io.rd}\n")

    assert(freeRegIdx === nPhyRegs.U, "freeRegIdx cannot be anything else")

    io.rdPhyReg.valid := false.B
    io.rdPhyReg.bits := DontCare
  }
}
