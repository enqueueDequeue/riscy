package io.riscy.stages

import chisel3.util.{PriorityMux, Valid, isPow2, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Input, Mem, Module, Mux, Output, PrintableHelper, RegInit, UInt, Vec, VecInit, Wire, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.InstructionQueue.{getMapIdx, getMapIdxH, getStationIdx}
import io.riscy.stages.signals.{IQSignals, Parameters}

class InstructionQueue()(implicit val params: Parameters) extends Module {
  val nPhyRegs = params.nPhyRegs
  val nEntries = params.nIQEntries

  assert(isPow2(nEntries))
  assert(isPow2(nPhyRegs))

  val io = IO(new Bundle {
    val instSignals = Input(Valid(IQSignals()))
    val wakeUpRegs = Input(UInt(nPhyRegs.W))
    // val wakeUpRegs = Input(Vec(nWakeUpPorts, Valid(UInt(log2Ceil(nPhyRegs).W))))
    val readyInstSignals = Output(Valid(IQSignals()))
  })

  val nEntriesLog = log2Ceil(nEntries)

  // everything below this index is deemed to be free
  val freeStationIdx = RegInit(0.U(log2Ceil(nEntries + 1).W))

  // freeStationsAlloc maintains if the freeStations has been
  // initialized or not. And, freeStations contain the index of
  // the station which is ready to be used.
  // todo: Decide between the UInt and Vec approaches
  val freeStationsAlloc = RegInit(0.U(nEntries.W))
  // val freeStationsAlloc = RegInit(VecInit(Seq.fill(nEntries) { false.B }))
  val freeStations = Mem(nEntries, UInt(log2Ceil(nEntries).W))

  // the actual instruction data to be stored
  val stations = Mem(nEntries, new IQSignals())

  // width = nEntries
  // height = nPhyRegs
  val map = RegInit(VecInit(Seq.fill(nPhyRegs * nEntries)(false.B)))
  val readyInsts = Wire(Vec(nEntries, Bool()))

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

     readyInsts(e) := !depPending
  }

  val tmpNextFreeStationIdx = Wire(UInt(log2Ceil(nEntries + 1).W))
  val nextFreeStationIdx = Wire(UInt(log2Ceil(nEntries + 1).W))

  val nextMapMask = Wire(UInt((nPhyRegs * nEntries).W))

  when(io.instSignals.valid && freeStationIdx < nEntries.U) {
    printf(cf"allocating inst in inst queue")

    val stationIdx = Wire(UInt(log2Ceil(nEntries).W))
    val actFreeStationIdx = getStationIdx(freeStationIdx, nEntries)

    // if the allocation has not been done yet, then use the freeStationIdx as it is
    stationIdx := Mux(freeStationsAlloc(actFreeStationIdx), freeStations(actFreeStationIdx), actFreeStationIdx)

    stations(stationIdx) := io.instSignals.bits

    // only depend if the register needs to be ready actually
    // todo: only mark as true iff rs1 needs to be read
    //  for example, if rs1PC is true, then there's no reason
    //  to mark this as true
    val rs1Mask = Wire(UInt((nPhyRegs * nEntries).W))
    val rs2Mask = Wire(UInt((nPhyRegs * nEntries).W))

    rs1Mask := 1.U << getMapIdxH(io.instSignals.bits.rename.rs1PhyReg, stationIdx, nEntriesLog)
    rs2Mask := 1.U << getMapIdxH(io.instSignals.bits.rename.rs2PhyReg, stationIdx, nEntriesLog)

    nextMapMask := rs1Mask | rs2Mask

    tmpNextFreeStationIdx := freeStationIdx + 1.U
  }.otherwise {
    tmpNextFreeStationIdx := freeStationIdx
    nextMapMask := 0.U
  }

  when(readyInstIdx.valid) {
    val actFreeStationIdx = getStationIdx(tmpNextFreeStationIdx, nEntries)

    freeStationsAlloc := freeStationsAlloc | (1.U << actFreeStationIdx).asUInt
    freeStations(actFreeStationIdx) := readyInstIdx.bits
    nextFreeStationIdx := tmpNextFreeStationIdx - 1.U
  }.otherwise {
    nextFreeStationIdx := tmpNextFreeStationIdx
  }

  for (r <- 0 until nPhyRegs) {
    for (e <- 0 until nEntries) {
      val idx = getMapIdx(r, e, nEntriesLog)
      map(idx) := Mux(io.wakeUpRegs(r), 0.U, map(idx) | nextMapMask(idx))
    }
  }

  freeStationIdx := nextFreeStationIdx

  io.readyInstSignals.valid := readyInstIdx.valid
  io.readyInstSignals.bits := Mux(readyInstIdx.valid, stations(readyInstIdx.bits), DontCare)
}

object InstructionQueue {
  def getStationIdx(idx: UInt, nEntries: Int): UInt = {
    idx(log2Ceil(nEntries) - 1, 0)
  }

  def getMapIdx(regIdx: Int, instIdx: Int, nEntriesLog: Int): Int = {
    (regIdx << nEntriesLog) + instIdx
  }

  def getMapIdxH(regIdx: UInt, instIdx: UInt, nEntriesLog: Int): UInt = {
    instIdx + (regIdx << nEntriesLog)
  }
}
