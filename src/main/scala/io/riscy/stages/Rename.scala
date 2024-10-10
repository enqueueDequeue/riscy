package io.riscy.stages

import chisel3.util.{PriorityMux, Valid, isPow2, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Input, Module, Mux, Output, PrintableHelper, Reg, RegInit, UInt, Vec, Wire, assert, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
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
  val rat = Reg(Vec(nArchRegs, UInt(log2Ceil(nPhyRegs).W)))
  val rrat = Reg(Vec(nArchRegs, UInt(log2Ceil(nPhyRegs).W)))
  val rratValid = RegInit(0.U(nArchRegs.W))

  io.rs1PhyReg := rat(io.rs1)
  io.rs2PhyReg := rat(io.rs2)

  when(io.retire.valid) {
    val freeRegister = rrat(io.retire.bits.arch)
    val freeRegisterValid = rratValid(io.retire.bits.arch)

    // mark the current register as free if it's valid
    printf(cf"Rename: freeing: $freeRegister, retiring: p${io.retire.bits.phy} @ a${io.retire.bits.arch}\n")

    rrat(io.retire.bits.arch) := io.retire.bits.phy
    rratValid := rratValid | (1.U << io.retire.bits.arch).asUInt

    when(freeRegisterValid) {
      freeRegs := (freeRegs | (1.U << freeRegister).asUInt)
    }

    io.deallocationIdx.valid := freeRegisterValid
    io.deallocationIdx.bits := freeRegister
  }.otherwise {
    io.deallocationIdx.valid := false.B
    io.deallocationIdx.bits := DontCare
  }

  when(io.rd.valid) {
    rat(io.rd.bits.arch) := io.rd.bits.phy
  }

  when(io.flush) {
    val nextFreeRegs = Wire(UInt(nPhyRegs.W))

    printf(cf"flushing")

    for (idx <- 0 until nArchRegs) {
      printf(cf"Rename: Copying ${rrat(idx)} valid: ${rratValid(idx)} -> ${rat(idx)}\n")
    }

    val occupiedRegs = Seq.tabulate(nArchRegs) { idx => Mux(rratValid(idx), (1.U << rrat(idx)).asUInt, 0.U) }

    nextFreeRegs := (~occupiedRegs.reduce(_ | _)).asUInt

    for (idx <- 0 until nArchRegs) {
      // idx is not valid meaning, that the no instruction using this dst
      // register has retired yet. So, no instruction after the flush should
      // read this register. So, even if the bits are pointing to a random index
      // it is still fine.
      // rat(idx) := Mux(rratValid(idx), rrat(idx), 0.U)

      rat(idx) := rrat(idx)
    }

    printf(cf"Rename: freeRegs: $freeRegs%b\n")
    printf(cf"Rename: freeRegs: ${nextFreeRegs}%b\n")

    freeRegs := nextFreeRegs
    io.allocatedRegs.valid := true.B
    io.allocatedRegs.bits := ~nextFreeRegs
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
