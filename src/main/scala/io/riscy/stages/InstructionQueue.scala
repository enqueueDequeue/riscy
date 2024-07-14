package io.riscy.stages

import chisel3.util.{PriorityMux, Valid, isPow2, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Input, Mem, Module, Mux, Output, PrintableHelper, RegInit, UInt, Vec, VecInit, Wire, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.InstructionQueue.{getActIdx, getMapIdx, getMapIdxH}
import io.riscy.stages.signals.IQSignals

class InstructionQueue(nEntries: Int, nPhyRegs: Int, nWakeUpPorts: Int) extends Module {
  assert(isPow2(nEntries))
  assert(isPow2(nPhyRegs))

  val io = IO(new Bundle {
    val instSignals = Input(Valid(IQSignals()))
    val wakeupRegs = Input(Vec(nWakeUpPorts, Valid(UInt(log2Ceil(nPhyRegs).W))))
    val readyInstSignals = Output(Valid(IQSignals()))
  })

  val nEntriesLog = log2Ceil(nEntries)

  // everything below this index is deemed to be free
  val freeStationIdx = RegInit(0.U(log2Ceil(nEntries + 1).W))

  // todo: use mem
  //  and not perform init?
  //  this is pretty important
  //  because, using vec is creating a huge
  //  Mux which can simply be avoided by using Mem
  //  Also, a freeStationsInitFlags can be maintained
  //  to default the value to the given idx
  val freeStations = RegInit(VecInit(Seq.tabulate(nEntries) { _.U(log2Ceil(nEntries).W) }))
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

  // all the instructions that depend on this
  // register are no longer dependent
  // todo: this logic is not being generated
  //  check why
  for (w <- 0 until nWakeUpPorts) {
    when(io.wakeupRegs(w).valid) {
      for (idx <- 0 until nEntries) {
        map(getMapIdxH(io.wakeupRegs(w).bits, idx.U, nEntriesLog)) := 0.U
      }
    }
  }

  // wake up instructions whose registers are ready
  for (idx <- 0 until nEntries) {
    var depPending = false.B

    for (reg <- 0 until nPhyRegs) {
      depPending = depPending | map(getMapIdx(reg, idx, nEntriesLog))
    }

     readyInsts(idx) := !depPending
  }

  val tmpNextFreeStationIdx = Wire(UInt(log2Ceil(nEntries + 1).W))
  val nextFreeStationIdx = Wire(UInt(log2Ceil(nEntries + 1).W))

  val nextMapMask = Wire(UInt((nPhyRegs * nEntries).W))

  when(io.instSignals.valid && freeStationIdx < nEntries.U) {
    printf(cf"allocating inst in inst queue")

    val stationIdx = Wire(UInt(log2Ceil(nEntries).W))

    stationIdx := freeStations(getActIdx(freeStationIdx, nEntries))

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
    freeStations(getActIdx(tmpNextFreeStationIdx, nEntries)) := readyInstIdx.bits
    nextFreeStationIdx := tmpNextFreeStationIdx - 1.U
  }.otherwise {
    nextFreeStationIdx := tmpNextFreeStationIdx
  }

  for (idx <- 0 until nPhyRegs * nEntries) {
    map(idx) := map(idx) | nextMapMask(idx)
  }

  freeStationIdx := nextFreeStationIdx

  io.readyInstSignals.valid := readyInstIdx.valid
  io.readyInstSignals.bits := Mux(readyInstIdx.valid, stations(readyInstIdx.bits), DontCare)
}

object InstructionQueue {
  def getActIdx(idx: UInt, nEntries: Int): UInt = {
    idx(log2Ceil(nEntries) - 1, 0)
  }

  def getMapIdx(regIdx: Int, instIdx: Int, nEntriesLog: Int): Int = {
    (regIdx << nEntriesLog) + instIdx
  }

  def getMapIdxH(regIdx: UInt, instIdx: UInt, nEntriesLog: Int): UInt = {
    instIdx + (regIdx << nEntriesLog)
  }
}
