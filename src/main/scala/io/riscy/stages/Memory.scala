package io.riscy.stages

import chisel3.util.{Cat, Fill, MuxLookup}
import chisel3.{Bool, Bundle, Input, Module, Mux, Output, PrintableHelper, UInt, fromIntToLiteral, fromIntToWidth, printf}
import io.riscy.stages.Memory.{getSizeBytes, getSizeBytesLit, isSigned}
import io.riscy.stages.MemRWSize
import io.riscy.stages.signals.Defaults.BIT_WIDTH

class Memory(addressWidth: Int, dataWidth: Int) extends Module {
  assert(dataWidth <= 8 * BIT_WIDTH)

  val io = IO(new Bundle {
    val address = Input(UInt(addressWidth.W))
    val writeSize = Input(MemRWSize())
    val writeData = Input(UInt(dataWidth.W))
    val readSize = Input(MemRWSize())
    val readData = Output(UInt(dataWidth.W))

    val dReadLen = Output(UInt((dataWidth / BIT_WIDTH).W))
    val dReadAddr = Output(UInt(addressWidth.W))
    val dReadValue = Input(UInt(dataWidth.W))
    val dWriteLen = Output(UInt((dataWidth / BIT_WIDTH).W))
    val dWriteAddr = Output(UInt(addressWidth.W))
    val dWriteValue = Output(UInt(dataWidth.W))
  })

  printf(cf"dReadValue: ${io.dReadValue}%x\n")

  io.dReadAddr := io.address
  io.dWriteAddr := io.address
  io.dWriteValue := io.writeData
  io.dReadLen := getSizeBytes(io.readSize)
  io.dWriteLen := getSizeBytes(io.writeSize)

  // We can either sign extend in this stage
  // OR we can sign extend in the memory subsystem
  // If this is done here, then no need to propagate
  // the sign information to memory subsystem
  // which makes it simpler
  // If this is done in the memory subsystem
  // which usually takes a clock cycle (in real world)
  // to return the value, sign extension might have yield
  // in lower cycle time in this stage
  val extension = Mux(isSigned(io.readSize), Fill(dataWidth, io.dReadValue(dataWidth - 1)), Fill(dataWidth, 0.U(1.W)))

  // If the read is only not dataWidth long,
  // then the read is expected to be present in
  // the most significant part of the word
  io.readData := MuxLookup(io.readSize, 0.U)(
    MemRWSize.all
      .filter { s => s != MemRWSize.BYTES_NO }
      .map { s =>
        val readSizeBytes = getSizeBytesLit(s)
        val dataOffsetBits = dataWidth - (BIT_WIDTH * readSizeBytes)

        s -> Cat(extension, io.dReadValue)(dataWidth + dataOffsetBits, dataOffsetBits)
      })
}

object Memory {
  def getSizeBytesLit(size: MemRWSize.Type): Int = {
    size.litValue.toInt >> 1
  }

  def getSizeBytes(size: MemRWSize.Type): UInt = {
    (size.asUInt >> 1.U).asUInt
  }

  def isSigned(size: MemRWSize.Type): Bool = {
    (size.asUInt & 1.U).orR
  }
}
