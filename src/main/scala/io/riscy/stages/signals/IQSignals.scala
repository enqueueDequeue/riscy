package io.riscy.stages.signals

import chisel3.Bundle

class IQSignals extends Bundle {
  val fetch = FetchSignals()
  val decode = DecodeSignals()
  val rename = RenameSignals()
}

object IQSignals {
  def apply(): IQSignals = {
    new IQSignals()
  }
}
