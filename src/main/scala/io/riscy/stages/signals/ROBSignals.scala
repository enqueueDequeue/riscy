package io.riscy.stages.signals

import chisel3.Bundle

class ROBSignals()(implicit params: Parameters) extends Bundle {
  val fetchSignals = FetchSignals()
  val decodeSignals = DecodeSignals()
  val renameSignals = RenameSignals()
}

object ROBSignals {
  def apply()(implicit params: Parameters): ROBSignals = {
    new ROBSignals()
  }
}
