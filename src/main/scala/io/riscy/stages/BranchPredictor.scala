package io.riscy.stages

import chisel3.util.{Cat, Fill, Valid, isPow2, log2Ceil}
import chisel3.{Bundle, Input, Mem, Module, Output, RegInit, UInt, fromIntToLiteral, fromIntToWidth, when}
import io.riscy.stages.BranchPredictor.{BRANCH, JAL, JALR, btbAddr, extractOpCode}
import io.riscy.stages.signals.Parameters

class BranchPredictor()(implicit params: Parameters) extends Module {
  val addrWidth = params.addrWidth
  val instWidth = params.instWidth
  val nBPredEntries = params.nBPredEntries
  val nRetStackEntries = params.nRetStackEntries

  // Using an ultra simple branch predictor
  // Prediction is just looking up the value in BTB.

  assert(isPow2(nBPredEntries))

  val io = IO(new Bundle {
    // only ask to predict branches
    val predict = Input(new Bundle {
      val addr = UInt(addrWidth.W)
      val inst = UInt(instWidth.W)
    })
    val predicted = Output(UInt(addrWidth.W))

    val update = Input(Valid(new Bundle {
      val branchInst = UInt(instWidth.W)
      val branchAddr = UInt(addrWidth.W)
      val targetAddr = UInt(addrWidth.W)
    }))
  })

  val btbValid = RegInit(0.U(nBPredEntries.W))
  val btb = Mem(nBPredEntries, UInt(addrWidth.W))

  val retStackPointer = RegInit(0.U(log2Ceil(nRetStackEntries).W))
  val retStack = Mem(nRetStackEntries, UInt(addrWidth.W))

  val predictionAddr = btbAddr(io.predict.addr)

  // identify the kind of instruction
  val opCode = extractOpCode(io.predict.inst)

  when(opCode === JAL.U) {
    val inst = io.predict.inst
    val immediate = Cat(Fill(44, inst(31)), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
    val predictedAddr = io.predict.addr + immediate

    // we should be returning to the instruction after JAL
    retStack(retStackPointer) := io.predict.addr + 4.U
    retStackPointer := retStackPointer + 1.U

    io.predicted := predictedAddr
  }.elsewhen(opCode === JALR.U) {
    // technically, should only use return stack
    // in case of using $ra as the return address
    // So, situations where jump is due to a vtable
    // entry in C++ or calling a function pointer,
    // this approach will be heavily mis-predicting.

    val predictedAddr = retStack(retStackPointer - 1.U)
    retStackPointer := retStackPointer - 1.U

    io.predicted := predictedAddr
  }.elsewhen(opCode === BRANCH.U) {
    when(btbValid(predictionAddr)) {
      io.predicted := btb(predictionAddr)
    }.otherwise {
      // if it's a backward branch, assume Taken
      // else assume not taken

      val inst = io.predict.inst
      val backward = inst(31)
      val immediate = Cat(Fill(52, inst(31)), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))

      when(backward) {
        io.predicted := io.predict.addr + immediate
      }.otherwise {
        io.predicted := io.predict.addr + 4.U
      }
    }
  }.otherwise {
    io.predicted := io.predict.addr + 4.U
  }

  when(io.update.valid) {
    val updateInstOpCode = extractOpCode(io.update.bits.branchInst)

    // only updating BTB, JAL and JALR are auto handled
    when(updateInstOpCode === BRANCH.U) {
      val updateBtbAddr = btbAddr(io.update.bits.branchAddr)

      btbValid := btbValid | (1.U << updateBtbAddr).asUInt
      btb(updateBtbAddr) := io.update.bits.targetAddr
    }
  }
}

object BranchPredictor {
  // Refer OpCode.scala
  val JAL = 0b1101111
  val JALR = 0b1100111
  val BRANCH = 0b1100011

  def extractOpCode(inst: UInt): UInt = {
    inst(6, 0)
  }

  def btbAddr(addr: UInt)(implicit params: Parameters): UInt = {
    addr(log2Ceil(params.nBPredEntries) - 1, 0)
  }
}
