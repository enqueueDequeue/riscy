package io.riscy.stages

import chisel3.util.{PriorityMux, Valid, isPow2, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Input, Mem, Module, Mux, Output, PrintableHelper, RegInit, UInt, Vec, VecInit, Wire, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.InstructionQueue.{getMapIdx, getMapIdxH}
import io.riscy.stages.signals.Parameters

class InstructionQueue()(implicit val params: Parameters) extends Module {
  val nPhyRegs = params.nPhyRegs
  val nEntries = params.nIQEntries
  val nROBEntries = params.nROBEntries

  assert(isPow2(nEntries))
  assert(isPow2(nPhyRegs))

  val io = IO(new Bundle {
    // allocate
    val allocate = Input(Bool())
    val allocatedIdx = Output(Valid(UInt(log2Ceil(nEntries).W)))

    // flush
    val flush = Input(Bool())

    // issue
    val instSignals = Input(Valid(new Bundle {
      val iqIdx = UInt(log2Ceil(nEntries).W)
      val robIdx = UInt(log2Ceil(nROBEntries).W)
      val rs1PhyReg = Valid(UInt(log2Ceil(nPhyRegs).W))
      val rs2PhyReg = Valid(UInt(log2Ceil(nPhyRegs).W))
    }))

    // wake up
    val wakeUpRegs = Input(UInt(nPhyRegs.W))

    // dispatch
    val readyInstSignals = Output(Valid(UInt(log2Ceil(nROBEntries).W)))
  })

  val nEntriesLog = log2Ceil(nEntries)

  // mark if the station is actually allocated or not
  // val allocations = RegInit(VecInit(Seq.fill(nEntries) { false.B }))
  val allocations = RegInit(0.U(nEntries.W))
  val occupancies = RegInit(0.U(nEntries.W))

  // the actual instruction data to be stored
  val stations = Mem(nEntries, UInt(log2Ceil(nROBEntries).W))

  // width = nEntries
  // height = nPhyRegs
  val map = RegInit(VecInit(Seq.fill(nPhyRegs * nEntries)(false.B)))
  val readyInsts = Wire(Vec(nEntries, Bool()))

  // last mux value reserved for invalid
  val freeStationIdx = PriorityMux(Seq.tabulate(nEntries + 1) { idx =>
    if (idx < nEntries) {
      val idxW = Wire(Valid(UInt(log2Ceil(nEntries).W)))

      idxW.valid := true.B
      idxW.bits := idx.U

      (!allocations(idx), idxW)
    } else {
      val idxW = Wire(Valid(UInt(log2Ceil(nEntries).W)))

      idxW.valid := false.B
      idxW.bits := DontCare

      (true.B, idxW)
    }
  })

  // last mux value reserved for invalid
  val readyInstIdx = PriorityMux(Seq.tabulate(nEntries + 1) { idx =>
    if (idx < nEntries) {
      val instSignals = Wire(Valid(UInt(log2Ceil(nEntries).W)))

      instSignals.valid := true.B
      instSignals.bits := idx.U

      (readyInsts(idx), instSignals)
    } else {
      require(idx == nEntries, "Error in the code, invalid idx detected")

      val instSignals = Wire(Valid(UInt(log2Ceil(nEntries).W)))

      instSignals.valid := false.B
      instSignals.bits := DontCare

      (true.B, instSignals)
    }
  })

  // wake up instructions whose registers are ready
  for (e <- 0 until nEntries) {
    val deps = VecInit(Seq.tabulate(nPhyRegs) { reg => map(getMapIdx(reg, e, nEntriesLog)) })
    val depPending = deps.reduceTree(_ | _)

    readyInsts(e) := !depPending & occupancies(e) & allocations(e)
  }

  val nextMapMask = Wire(UInt((nPhyRegs * nEntries).W))

  val nextAllocationsIntermediate = Wire(UInt(nEntries.W))
  val nextAllocations = Wire(UInt(nEntries.W))

  val nextOccupanciesIntermediate = Wire(UInt(nEntries.W))
  val nextOccupancies = Wire(UInt(nEntries.W))

  when(readyInstIdx.valid) {
    printf(cf"IQ: Freeing idx: ${readyInstIdx.bits}\n")
    nextAllocationsIntermediate := allocations & (~(1.U << readyInstIdx.bits)).asUInt
    nextOccupanciesIntermediate := occupancies & (~(1.U << readyInstIdx.bits)).asUInt
  }.otherwise {
    nextAllocationsIntermediate := allocations
    nextOccupanciesIntermediate := occupancies
  }

  when(io.allocate && freeStationIdx.valid) {
    val stationIdx = freeStationIdx.bits

    printf(cf"IQ: allocating idx: $stationIdx\n")

    io.allocatedIdx.valid := true.B
    io.allocatedIdx.bits := stationIdx

    nextAllocations := nextAllocationsIntermediate | (1.U << stationIdx).asUInt
  }.otherwise {
    printf(cf"IQ: not allocating\n")

    io.allocatedIdx.valid := false.B
    io.allocatedIdx.bits := DontCare

    nextAllocations := nextAllocationsIntermediate
  }

  when(io.instSignals.valid) {
    val stationIdx = io.instSignals.bits.iqIdx

    stations(stationIdx) := io.instSignals.bits.robIdx

    // only depend if the register needs to be ready actually
    val rs1Mask = Wire(UInt((nPhyRegs * nEntries).W))
    val rs2Mask = Wire(UInt((nPhyRegs * nEntries).W))

    when(io.instSignals.bits.rs1PhyReg.valid) {
      rs1Mask := 1.U << getMapIdxH(io.instSignals.bits.rs1PhyReg.bits, stationIdx, nEntriesLog)
    }.otherwise {
      rs1Mask := 0.U
    }

    when(io.instSignals.bits.rs2PhyReg.valid) {
      rs2Mask := 1.U << getMapIdxH(io.instSignals.bits.rs2PhyReg.bits, stationIdx, nEntriesLog)
    }.otherwise {
      rs2Mask := 0.U
    }

    nextMapMask := rs1Mask | rs2Mask
    nextOccupancies := nextOccupanciesIntermediate | (1.U << stationIdx).asUInt
  }.otherwise {
    nextMapMask := 0.U
    nextOccupancies := nextOccupanciesIntermediate
  }

  for (r <- 0 until nPhyRegs) {
    for (e <- 0 until nEntries) {
      val idx = getMapIdx(r, e, nEntriesLog)
      map(idx) := Mux(io.wakeUpRegs(r), 0.U, map(idx) | nextMapMask(idx))
    }
  }

  allocations := nextAllocations
  occupancies := nextOccupancies

  when(io.flush) {
    allocations := 0.U
  }

  printf(cf"IQ: allocations: |$allocations%b|\n")
  printf(cf"IQ: readyInsts: $readyInsts\n")
  printf(cf"IQ: readyInstIdx: $readyInstIdx\n")

  io.readyInstSignals.valid := readyInstIdx.valid
  io.readyInstSignals.bits := Mux(readyInstIdx.valid, stations(readyInstIdx.bits), DontCare)
}

object InstructionQueue {
  def getMapIdx(regIdx: Int, instIdx: Int, nEntriesLog: Int): Int = {
    (regIdx << nEntriesLog) + instIdx
  }

  def getMapIdxH(regIdx: UInt, instIdx: UInt, nEntriesLog: Int): UInt = {
    instIdx + (regIdx << nEntriesLog)
  }
}
