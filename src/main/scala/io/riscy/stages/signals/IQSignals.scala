package io.riscy.stages.signals

import chisel3.Bundle

class IQSignals()(implicit params: Parameters) extends Bundle {
  val fetch = FetchSignals()
  val decode = DecodeSignals()
  val rename = RenameSignals()
}

object IQSignals {
  def apply()(implicit params: Parameters): IQSignals = {
    new IQSignals()
  }
}
