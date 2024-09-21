package io.riscy.stages

import chisel3.util.{PriorityMux, Valid, isPow2, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Input, Module, Mux, Output, PrintableHelper, RegInit, UInt, Vec, VecInit, Wire, assert, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.signals.Parameters

class Rename()(implicit val params: Parameters) extends Module {
  val nPhyRegs = params.nPhyRegs
  val nArchRegs = params.nArchRegs

  require(isPow2(nPhyRegs))

  val io = IO(new Bundle {
    val allocate = Input(Bool())
    val allocatedIdx = Output(Valid(UInt(log2Ceil(nPhyRegs).W)))

    // Allocated regs is only returned during a flush
    // Can be used to set/reset wake up registers
    val allocatedRegs = Output(Valid(UInt(nPhyRegs.W)))
    val deallocationIdx = Output(Valid(UInt(log2Ceil(nPhyRegs).W)))

    val rs1 = Input(UInt(log2Ceil(nArchRegs).W))
    val rs2 = Input(UInt(log2Ceil(nArchRegs).W))
    val rd = Input(Valid(new Bundle {
      val arch = UInt(log2Ceil(nArchRegs).W)
      val phy = UInt(log2Ceil(nPhyRegs).W)
    }))

    val rs1PhyReg = Output(UInt(log2Ceil(nPhyRegs).W))
    val rs2PhyReg = Output(UInt(log2Ceil(nPhyRegs).W))

    val retire = Input(Valid(new Bundle {
      val arch = UInt(log2Ceil(nArchRegs).W)
      val phy = UInt(log2Ceil(nPhyRegs).W)
    }))

    val flush = Input(Bool())
  })

  // A good reference:
  // https://cs.stackexchange.com/questions/103433/when-to-free-physical-registers

  // right after reset
  val freeRegs = RegInit((~0.U(nPhyRegs.W)).asUInt)

  val freeRegIdx = PriorityMux(Seq.tabulate(nPhyRegs + 1) { idx =>
    assert(idx <= nPhyRegs)

    if (idx < nPhyRegs) {
      val initSignals = Wire(Valid(UInt(log2Ceil(nPhyRegs).W)))

      initSignals.valid := true.B
      initSignals.bits := idx.U

      (freeRegs(idx), initSignals)
    } else {
      val initSignals = Wire(Valid(UInt(log2Ceil(nPhyRegs).W)))

      initSignals.valid := false.B
      initSignals.bits := DontCare

      (true.B, initSignals)
    }
  })

  // register alias tables
  val rat = RegInit(VecInit(Seq.fill(nArchRegs)(0.U(log2Ceil(nPhyRegs).W))))
  val retirementRat = RegInit(VecInit(Seq.fill(nArchRegs) {
    val initSignals = Wire(Valid(UInt(log2Ceil(nPhyRegs).W)))

    initSignals.valid := false.B
    initSignals.bits := 0.U

    initSignals
  }))

  // helpers along the way

  io.rs1PhyReg := rat(io.rs1)
  io.rs2PhyReg := rat(io.rs2)

  when(io.retire.valid) {
    val freeRegister = retirementRat(io.retire.bits.arch)

    // mark the current register as free if it's valid
    printf(cf"Rename: freeing: $freeRegister, retiring: p${io.retire.bits.phy} @ a${io.retire.bits.arch}\n")

    when(freeRegister.valid) {
      freeRegs := (freeRegs | (1.U << freeRegister.bits).asUInt)
    }

    retirementRat(io.retire.bits.arch).valid := true.B
    retirementRat(io.retire.bits.arch).bits := io.retire.bits.phy

    io.deallocationIdx := freeRegister
  }.otherwise {
    io.deallocationIdx.valid := false.B
    io.deallocationIdx.bits := DontCare
  }

  when(io.rd.valid) {
    rat(io.rd.bits.arch) := io.rd.bits.phy
  }

  when(io.flush) {
    val nextFreeRegs = Wire(Vec(nPhyRegs, Bool()))

    for (idx <- 0 until nArchRegs) {
      printf(cf"Rename: Copying ${retirementRat(idx)} -> ${rat(idx)}\n")
    }

    for (idx <- 0 until nPhyRegs) {
      nextFreeRegs(idx) := true.B
    }

    for (idx <- 0 until nArchRegs) {
      when(retirementRat(idx).valid) {
        nextFreeRegs(retirementRat(idx).bits) := false.B
      }
    }

    for (idx <- 0 until nArchRegs) {
      rat(idx) := Mux(retirementRat(idx).valid, retirementRat(idx).bits, 0.U)
    }

    printf(cf"Rename: freeRegs: $freeRegs%b\n")
    printf(cf"Rename: freeRegs: ${nextFreeRegs.asUInt}%b\n")

    freeRegs := nextFreeRegs.asUInt
    io.allocatedRegs.valid := true.B
    io.allocatedRegs.bits := ~nextFreeRegs.asUInt
  }.otherwise {
    io.allocatedRegs.valid := false.B
    io.allocatedRegs.bits := DontCare
  }

  when(freeRegIdx.valid && io.allocate) {
    assert(!io.flush, "Illegal state detected, cannot be allocating during flush")

    val phyReg = freeRegIdx.bits

    printf(cf"Allocating p$phyReg <- $freeRegIdx\n")

    freeRegs := freeRegs & (~(1.U << freeRegIdx.bits)).asUInt

    io.allocatedIdx.valid := true.B
    io.allocatedIdx.bits := phyReg
  }.otherwise {
    printf(cf"Rename: allocate: ${io.allocate} freeRegs = $freeRegs%x\n")

    io.allocatedIdx.valid := false.B
    io.allocatedIdx.bits := DontCare
  }
}
