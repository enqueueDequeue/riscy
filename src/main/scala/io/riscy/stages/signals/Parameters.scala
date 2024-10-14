package io.riscy.stages.signals

case class Parameters(nArchRegs: Int,
                      nPhyRegs: Int,
                      instWidth: Int,
                      wordWidth: Int,
                      dataWidth: Int,
                      addrWidth: Int,
                      bitWidth: Int,
                      nIQEntries: Int,
                      nROBEntries: Int,
                      nLDQEntries: Int,
                      nSTQEntries: Int)
