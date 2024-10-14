package io.riscy.stages.signals

import chisel3.util.log2Ceil
import chisel3.{Bundle, UInt, fromIntToWidth}

class ROBSignals()(implicit params: Parameters) extends Bundle {
  val robIdx = UInt(log2Ceil(params.nROBEntries).W)

  val fetchSignals = FetchSignals()
  val decodeSignals = DecodeSignals()
  val renameSignals = RenameSignals()
  val allocSignals = AllocSignals()
}
