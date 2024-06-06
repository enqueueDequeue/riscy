package io.riscy

import chisel3.util.{Cat, Fill, MuxLookup}
import chisel3.{Bool, Bundle, ChiselEnum, Input, Module, Mux, Output, PrintableHelper, UInt, fromIntToLiteral, fromIntToWidth, printf}
import io.riscy.Memory.{BIT_WIDTH, getSizeBytes, getSizeBytesLit, isSigned}

class Memory(addressWidth: Int, dataWidth: Int) extends Module {
  assert(dataWidth <= 8 * BIT_WIDTH)

  val io = IO(new Bundle {
    val address = Input(UInt(addressWidth.W))
    val writeSize = Input(Size())
    val writeData = Input(UInt(dataWidth.W))
    val readSize = Input(Size())
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
    Size.all
      .filter { s => s != Size.BYTES_NO }
      .map { s =>
        val readSizeBytes = getSizeBytesLit(s)
        val dataOffsetBits = dataWidth - (BIT_WIDTH * readSizeBytes)

        s -> Cat(extension, io.dReadValue)(dataWidth + dataOffsetBits, dataOffsetBits)
      })
}

object Size extends ChiselEnum {
  val BYTES_NO = Value(0x00.U)
  val BYTES_1U = Value(0x02.U)
  val BYTES_1S = Value(0x03.U)
  val BYTES_2U = Value(0x04.U)
  val BYTES_2S = Value(0x05.U)
  val BYTES_4U = Value(0x08.U)
  val BYTES_4S = Value(0x09.U)
  val BYTES_8U = Value(0x10.U)
  val BYTES_8S = Value(0x11.U)
}

object Memory {
  val BIT_WIDTH = 8

  def getSizeBytesLit(size: Size.Type): Int = {
    size.litValue.toInt >> 1
  }

  def getSizeBytes(size: Size.Type): UInt = {
    (size.asUInt >> 1.U).asUInt
  }

  def isSigned(size: Size.Type): Bool = {
    (size.asUInt & 1.U).orR
  }
}
