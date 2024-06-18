package io.riscy

import chisel3.{VecInit, fromBooleanToLiteral, fromIntToLiteral, fromLongToLiteral, when}
import chiseltest.RawTester.test
import chiseltest.simulator.WriteVcdAnnotation
import chiseltest.{testableBool, testableClock, testableData}
import circt.stage.ChiselStage
import io.riscy.stages.PhyRegs

import scala.util.control.Breaks.{break, breakable}

object Main {
  class MockICache extends Cache(64, 32) {
    io.read.ready := true.B
    io.write.ready := true.B

    val mockInstructions = VecInit(Seq.range(0, 1024, 1).map(a => a.U))

    when(io.read.fire) {
      io.readValue.enq(mockInstructions(io.read.bits(9, 0)))
    }.otherwise {
      io.readValue.noenq()
    }
  }

  def main(args: Array[String]): Unit = {
    /*
    test(new PhyRegs(128)) { dut =>
      dut.io.rdEn.poke(true.B)

      dut.io.rd.poke(1.U)
      dut.io.rdValue.poke(42.U)

      dut.clock.step()

      dut.io.rs1.poke(1.U)
      dut.io.rs2.poke(1.U)

      dut.io.rs1Value.expect(42.U)
      dut.io.rs2Value.expect(42.U)

      dut.io.rd.poke(2.U)
      dut.io.rdValue.poke(43.U)

      dut.clock.step()

      dut.io.rdValue.poke(44.U)
      dut.io.rs1.poke(2.U)
      dut.io.rs1Value.expect(43.U)

      dut.clock.step()

      dut.io.rs1Value.expect(44.U)

      dut.io.rd.poke(0.U)
      dut.io.rdValue.poke(41.U)

      dut.clock.step()

      dut.io.rs1.poke(0.U)
      dut.io.rs1Value.expect(0.U)
    }

    test(new RegCache(64, 32)) { dut =>
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.address.poke(10.U)
      dut.io.write.bits.content.poke(42.U)

      dut.clock.step()

      dut.io.read.valid.poke(true.B)
      dut.io.read.bits.poke(10.U)

      while(!dut.io.readValue.valid.peek().litToBoolean) {
        dut.clock.step()
      }

      dut.io.readValue.bits.expect(42.U)
    }

    test(new RegCache(64, 32)) { iCache =>
      for(i <- 0 until 1024) {
        iCache.io.write.ready.expect(true.B)
        iCache.io.write.valid.poke(true.B)
        iCache.io.write.bits.address.poke(i.U)
        iCache.io.write.bits.content.poke(i.U)

        iCache.clock.step()
      }

      for(i <- 0 until 1024) {
        iCache.io.readValue.ready.poke(true.B)

        if (iCache.io.read.ready.peek().litToBoolean) {
          iCache.io.read.valid.poke(true.B)
          iCache.io.read.bits.poke(i.U)

          while(!iCache.io.readValue.valid.peek().litToBoolean) {
            iCache.clock.step()
          }

          iCache.io.readValue.bits.expect(i.U)
        }
      }
    }

    test(new InOrderCPU(), Seq(WriteVcdAnnotation)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.clock.step()

      val instructions = Seq(
        0x00800293, // ADDI x5, x0, 8
        0x0002a303, // LW x6, 0(x5)
        0x00a30313, // ADDI x6, x6, 10
        0x0062a023  // SW x6, 0(x5)
      )

      dut.io.iReadAddr.ready.poke(true.B)

      // ADDI x5, x0, 8
      dut.io.iReadAddr.valid.expect(true.B)
      dut.io.iReadAddr.bits.expect(0.U)

      dut.io.iReadValue.valid.poke(true.B)
      dut.io.iReadValue.bits.poke(instructions(0).asUInt)
      dut.io.iReadValue.ready.expect(true.B)

      dut.clock.step()

      // LW x6, 0(x5)
      dut.io.iReadAddr.valid.expect(true.B)
      dut.io.iReadAddr.bits.expect(4.U)

      dut.io.iReadValue.ready.expect(true.B)
      dut.io.iReadValue.valid.poke(true.B)
      dut.io.iReadValue.bits.poke(instructions(1).asUInt)

      val readValue = 10L << 32

      println("******** input: " + readValue + " ********")

      dut.io.dReadAddr.expect(8.U)
      dut.io.dReadLen.expect(4.U)
      dut.io.dReadValue.poke(readValue.U)

      dut.clock.step()

      dut.io.dReadValue.poke(0.U)

      // ADDI x6, x6, 10
      dut.io.iReadAddr.valid.expect(true.B)
      dut.io.iReadAddr.bits.expect(8.U)

      dut.io.iReadValue.ready.expect(true.B)
      dut.io.iReadValue.valid.poke(true.B)
      dut.io.iReadValue.bits.poke(instructions(2).asUInt)

      dut.clock.step()

      // SW x6, 0(x5)
      dut.io.iReadAddr.valid.expect(true.B)
      dut.io.iReadAddr.bits.expect(12.U)

      dut.io.iReadValue.ready.expect(true.B)
      dut.io.iReadValue.valid.poke(true.B)
      dut.io.iReadValue.bits.poke(instructions(3).asUInt)

      dut.io.dWriteAddr.expect(8.U)
      dut.io.dWriteLen.expect(4.U)
      dut.io.dWriteValue.expect(20.U)

      dut.clock.step()
    }
    */

    test(new InOrderPipelinedCPU(), Seq(WriteVcdAnnotation)) { dut =>
      println("%%%%%%% Testing memory model %%%%%%%")

      val instructions = Seq(
//          0x00c00393L, // ADDI x7, x0, 12
//          0x00000313L, // ADDI x6, x0, 0
//          0x00528293L, // ADDI x5, x5, 5
//          0x00530333L, // ADD x6, x6, x5
//          0xfff28293L, // ADDI x5, x5, -1
//          0x0063a023L, // SW x6, 0(x7)

        0x00c00393L, // ADDI x7, x0, 12
        0x00000313L, // ADDI x6, x0, 0
        0x00528293L, // ADDI x5, x5, 5
        0x00530333L, // ADD x6, x6, x5
        0xfff28293L, // ADDI x5, x5, -1
        0x00028463L, // BEQ x5, x0, 8
        0xff5ff06fL, // JAL x0, -12
        0x0063a023L, // SW x6, 0(x7)
//        // buffering a few instructions at the end
        0x00000033L, // ADD x0, x0, x0
        0x00000033L, // ADD x0, x0, x0
        0x00000033L, // ADD x0, x0, x0
        0x00000033L, // ADD x0, x0, x0
        0x00000033L, // ADD x0, x0, x0
        0x00000033L, // ADD x0, x0, x0
      )

      val memory = new Array[Byte](64)

      for (i <- memory.indices) {
        memory(i) = i.toByte
      }

      println(memory.mkString("memory: (", ", ", ")"))

      dut.io.iReadAddr.ready.poke(true.B)

      var instructionsExecuted = 0
      breakable {
        while (true) {
          dut.io.iReadAddr.valid.expect(true.B)

          val inst_idx = dut.io.iReadAddr.bits.peek().litValue.toInt / 4

          if (inst_idx >= instructions.length) {
            break
          }

          instructionsExecuted += 1

          println("%%%%%%% executing inst_idx: " + inst_idx + " %%%%%%%")

          dut.io.iReadValue.valid.poke(true.B)
          dut.io.iReadValue.bits.poke(instructions(inst_idx).asUInt)
          dut.io.iReadValue.ready.expect(true.B)

          var readValue = 0L
          val readAddr = dut.io.dReadAddr.peek().litValue.toInt
          val readLen = dut.io.dReadLen.peek().litValue.toInt

          for (readOffset <- 0 until readLen) {
            readValue = readValue << 8
            readValue = readValue | memory(readAddr + readOffset)
          }

          println(s"address: 0x${readAddr.toHexString}")
          println(s"readValue: 0x${readValue.toHexString}")

          readValue = readValue << (64 - 8 * readLen)

          dut.io.dReadValue.poke(readValue.U)

          val writeAddr = dut.io.dWriteAddr.peek().litValue.toInt
          val writeLen = dut.io.dWriteLen.peek().litValue.toInt
          var writeData = dut.io.dWriteValue.peek().litValue.toLong

          println(s"writeData: 0x${writeData.toHexString}")

          for (writeOffset <- 0 until writeLen) {
            memory(writeAddr + writeLen - writeOffset - 1) = (writeData & 0xFF).toByte
            writeData = writeData >> 8
          }

          dut.clock.step()
        }
      }

      println(s"%%%%%%% completed, instructions executed: $instructionsExecuted %%%%%%%")
      println(memory.mkString("memory: (", ", ", ")"))
    }

    /*
    println(
      ChiselStage.emitSystemVerilog(
        gen = new InOrderPipelinedCPU(),
        firtoolOpts = Array("-disable-all-randomization")
      )
    )
    */
  }
}
