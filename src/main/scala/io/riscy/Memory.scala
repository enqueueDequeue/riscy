package io.riscy

import chisel3.util.{Cat, Fill, log2Ceil}
import chisel3.{Bool, Bundle, ChiselEnum, Input, Module, Output, UInt, fromIntToLiteral, fromIntToWidth, when}
import io.riscy.Memory.{BIT_WIDTH, getSizeBytes, getSizeLit, isSigned}

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

  // offset after which the data actually starts
  // if the read length is only
  val dataOffsetBits = dataWidth - (BIT_WIDTH * getSizeLit(io.readSize))

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
  when(isSigned(io.readSize)) {
    io.readData := Cat(Fill(dataWidth, io.dReadValue(dataWidth - 1)), io.dReadValue)(dataWidth + dataOffsetBits, dataOffsetBits)
  }.otherwise {
    io.readData := Cat(Fill(dataWidth, 0.U(1.W)), io.dReadValue)(dataWidth + dataOffsetBits, dataOffsetBits)
  }
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

  def getSizeLit(size: Size.Type): BigInt = {
    4
  }

  def getSizeBytes(size: Size.Type): UInt = {
    (size.asUInt >> 1.U).asUInt
  }

  def isSigned(size: Size.Type): Bool = {
    (size.asUInt & 1.U).orR
  }
}
