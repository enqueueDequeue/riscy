package io.riscy

import chisel3.{VecInit, fromBooleanToLiteral, fromIntToLiteral, when}
import chiseltest.RawTester.test
import chiseltest.{testableBool, testableClock, testableData}
import circt.stage.ChiselStage

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

    test(new InOrderCPU()) { dut =>
      val instructions = Seq(
        0x00828293, // ADDI x5, x5, 8
        0x0002a303, // LW x6, 0(x5)
        0x00a30313, // ADDI x6, x6, 10
        0x0062a023  // SW x6, 0(x5)
      )

      dut.io.iReadAddr.ready.poke(true.B)
      dut.io.iReadAddr.valid.expect(true.B)
      dut.io.iReadAddr.bits.expect(0.U)

      dut.io.iReadValue.valid.poke(true.B)
      dut.io.iReadValue.bits.poke(instructions(0).asUInt)
      dut.io.iReadValue.ready.expect(true.B)

      dut.clock.step()

      dut.io.iReadAddr.valid.expect(true.B)
      dut.io.iReadAddr.bits.expect(4.U)

      dut.io.iReadValue.ready.expect(true.B)
      dut.io.iReadValue.valid.poke(true.B)
      dut.io.iReadValue.bits.poke(instructions(1).asUInt)

      dut.io.dReadAddr.expect(8.U)
      dut.io.dReadLen.expect(4.U)
      dut.io.dReadValue.poke(10.U)

      dut.clock.step()

      dut.io.iReadAddr.ready := true.B

      dut.io.iReadAddr.valid.expect(true.B)
      dut.io.iReadAddr.bits.expect(0.U)

      dut.io.iReadValue.ready.expect(true.B)
      dut.io.iReadValue.valid.poke(true.B)
      dut.io.iReadValue.bits.poke(instructions(2).asUInt)

      dut.clock.step()

      dut.io.iReadAddr.valid.expect(true.B)
      dut.io.iReadAddr.bits.expect(4.U)

      dut.io.iReadValue.ready.expect(true.B)
      dut.io.iReadValue.valid.poke(true.B)
      dut.io.iReadValue.bits.poke(instructions(1).asUInt)

      dut.io.dWriteAddr.expect(8.U)
      dut.io.dWriteLen.expect(4.U)
      dut.io.dWriteValue.poke(20.U)

      dut.clock.step()
    }

    println(
      ChiselStage.emitSystemVerilog(
        gen = new InOrderCPU(),
        firtoolOpts = Array("-disable-all-randomization")
      )
    )
  }
}
