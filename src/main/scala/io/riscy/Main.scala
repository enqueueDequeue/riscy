package io.riscy

import chisel3.{Bundle, DontCare, IO, Input, Module, Output, UInt, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, fromLongToLiteral, fromStringToLiteral}
import chiseltest.RawTester.test
import chiseltest.simulator.WriteVcdAnnotation
import chiseltest.{VerilatorBackendAnnotation, testableBool, testableClock, testableData}
import circt.stage.ChiselStage
import io.riscy.stages.signals.Parameters
import io.riscy.stages.{InstructionQueue, MemRWDirection, MemRWSize, MemoryO3, PhyRegs, ROB, Rename}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.control.Breaks.{break, breakable}

object Main {

  def toInt(memory: Array[Byte]): Int = {
    var memValue: Int = 0

    require(memory.length == 4)

    for (idx <- 0 until 4) {
      var value = memory(idx).toInt

      value = value << 24
      value = value >>> 24

      memValue = memValue << 8
      memValue = memValue | value
    }

    memValue
  }

  def toLong(memory: Array[Byte]): Long = {
    var memValue: Long = 0

    require(memory.length == 8)

    for (idx <- 0 until 8) {
      var value = memory(idx).toLong

      value = value << 56
      value = value >>> 56

      memValue = memValue << 8
      memValue = memValue | value
    }

    memValue
  }

  def printMem(memory: Array[Byte], wordLen: Int = 4, wordGen: Array[Byte] => Number = toInt): Unit = {
    val factor = 256

    for (idxX <- memory.indices by factor) {
      for (idxY <- 0 until factor by wordLen) {
        val idx = idxX + idxY
        val memValue = wordGen(memory.slice(idx, idx + wordLen))

        print(s"$memValue,")
      }
      println("")
    }
    println("")
  }

  def getParams(nArchRegs: Int = 32,
                nPhyRegs: Int = 128,
                instWidth: Int = 32,
                wordWidth: Int = 32,
                dataWidth: Int = 64,
                addrWidth: Int = 64,
                bitWidth: Int = 8,
                nIQEntries: Int = 64,
                nROBEntries: Int = 64,
                nLDQEntries: Int = 32,
                nSTQEntries: Int = 16): Parameters = {
    Parameters(nArchRegs, nPhyRegs, instWidth, wordWidth, dataWidth, addrWidth, bitWidth, nIQEntries, nROBEntries, nLDQEntries, nSTQEntries)
  }

  def main(args: Array[String]): Unit = {

    /*
    {
      implicit val params = getParams()

      test(new PhyRegs()) { dut =>
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
        dut.io.rs1Value.expect(41.U)
      }
    }

    {
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
    }

    {
      implicit val params = getParams(nPhyRegs = 4)

      test(new Rename()) { dut =>
        println(s"Testing Rename")

        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.expect(0.U)
        dut.clock.step()

        dut.io.rd.valid.poke(true.B)
        dut.io.rd.bits.arch.poke(1.U)
        dut.io.rd.bits.phy.poke(0.U)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.expect(1.U)
        dut.clock.step()

        dut.io.rs1.poke(1.U)
        dut.io.rs1PhyReg.expect(0.U)
        dut.io.rd.valid.poke(true.B)
        dut.io.rd.bits.arch.poke(1.U)
        dut.io.rd.bits.phy.poke(1.U)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.expect(2.U)
        dut.clock.step()

        dut.io.rs1.poke(1.U)
        dut.io.rs1PhyReg.expect(1.U)
        dut.io.rd.valid.poke(true.B)
        dut.io.rd.bits.arch.poke(2.U)
        dut.io.rd.bits.phy.poke(2.U)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.expect(3.U)
        dut.clock.step()

        dut.io.rs1.poke(1.U)
        dut.io.rs1PhyReg.expect(1.U)
        dut.io.rs2.poke(2.U)
        dut.io.rs2PhyReg.expect(2.U)
        dut.io.rd.valid.poke(true.B)
        dut.io.rd.bits.arch.poke(3.U)
        dut.io.rd.bits.phy.poke(3.U)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(false.B)
        dut.clock.step()

        dut.io.rs1.poke(3.U)
        dut.io.rs1PhyReg.expect(3.U)
        dut.io.rs2.poke(2.U)
        dut.io.rs2PhyReg.expect(2.U)
        dut.io.rd.valid.poke(false.B)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(false.B)
        dut.clock.step()

        // retire the physical register
        dut.io.retire.valid.poke(true.B)
        dut.io.retire.bits.arch.poke(1.U)
        dut.io.retire.bits.phy.poke(0.U)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(false.B)
        dut.clock.step()

        // retire again
        // this will free the previous value from RRat
        dut.io.retire.valid.poke(true.B)
        dut.io.retire.bits.arch.poke(1.U)
        dut.io.retire.bits.phy.poke(1.U)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(false.B)
        dut.clock.step()

        dut.io.retire.valid.poke(true.B)
        dut.io.retire.bits.arch.poke(2.U)
        dut.io.retire.bits.phy.poke(2.U)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.expect(0.U)
        dut.clock.step()

        dut.io.rd.valid.poke(true.B)
        dut.io.rd.bits.arch.poke(1.U)
        dut.io.rd.bits.phy.poke(0.U)
        dut.io.retire.valid.poke(true.B)
        dut.io.retire.bits.arch.poke(3.U)
        dut.io.retire.bits.phy.poke(3.U)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(false.B)
        dut.clock.step()

        dut.io.rs1.poke(3.U)
        dut.io.rs1PhyReg.expect(3.U)
        dut.io.rs2.poke(1.U)
        dut.io.rs2PhyReg.expect(0.U)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(false.B)
        dut.clock.step()

        dut.io.flush.poke(true.B)
        dut.io.allocate.poke(false.B)
        dut.clock.step()

        dut.io.flush.poke(false.B)
        dut.io.rs1.poke(1.U)
        dut.io.rs1PhyReg.expect(1.U)
        dut.io.rs1.poke(2.U)
        dut.io.rs1PhyReg.expect(2.U)
        dut.io.rs1.poke(3.U)
        dut.io.rs1PhyReg.expect(3.U)
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.expect(0.U)
        dut.clock.step()

        println(s"Renaming test completed")
      }
    }

    {
      implicit val params = getParams()

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

      test(new InOrderPipelinedCPU(), Seq(WriteVcdAnnotation)) { dut =>
        println("%%%%%%% Testing memory model %%%%%%%")

        val instructions = Seq(
          0x00200393L, // ADDI x7, x0, 2
          0x00702023L, // SW x7, 0(x0)
          0x00002383L, // LW x7, 0(x0)
          0x007383b3L, // ADD x7, x7, x7
          0x007383b3L, // ADD x7, x7, x7
          0x00c38393L, // ADDI x7, x7, 12
          0x00000313L, // ADDI x6, x0, 0
          0x00528293L, // ADDI x5, x5, 5
          0x00530333L, // ADD x6, x6, x5
          0xfff28293L, // ADDI x5, x5, -1
          0x00028463L, // BEQ x5, x0, 8
          0xff5ff06fL, // JAL x0, -12
          0x0063a023L, // SW x6, 0(x7)
          // buffering a few instructions at the end
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
        )

        val memory = new Array[Byte](64)

        for (i <- memory.indices) {
          memory(i) = 0.toByte
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
              var memValue = memory(readAddr + readOffset).toInt

              memValue = (memValue << 24) >>> 24

              readValue = readValue << 8
              readValue = readValue | memValue
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
              writeData = writeData >>> 8
            }

            dut.clock.step()
          }
        }

        println(s"%%%%%%% completed, instructions executed: $instructionsExecuted %%%%%%%")
        println(memory.mkString("memory: (", ", ", ")"))

        require(toInt(memory.slice(20, 24)) == 15)
      }

      test(new InOrderPipelinedCPU(), Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        println("%%%%%%% Testing Quick Sort: RV32 %%%%%%%")

        val instructions = Seq(
          // set return address to out of bounds
          0x40000093L,
          // update stack size = 1024
          0x40000113L,
          // actual instructions
          0xfd010113L,
          0x02112623L,
          0x00100513L,
          0x00a12223L,
          0x00700513L,
          0x00a12423L,
          0x00600513L,
          0x00a12623L,
          0x00500513L,
          0x00a12823L,
          0x00900513L,
          0x00a12a23L,
          0x00400513L,
          0x00a12c23L,
          0x00300513L,
          0x00a12e23L,
          0x00200513L,
          0x02a12023L,
          0x02012223L,
          0x00800513L,
          0x02a12423L,
          0x00900593L,
          0x00410613L,
          0x00000513L,
          0x010000efL,
          0x02c12083L,
          0x03010113L,
          0x00008067L,
          0xfe010113L,
          0x00112e23L,
          0x00812c23L,
          0x00912a23L,
          0x01212823L,
          0x01312623L,
          0x01412423L,
          0x00060a13L,
          0x00058913L,
          0x00259993L,
          0x00c989b3L,
          0x02c0006fL,
          0x00249593L,
          0x014585b3L,
          0x0009a603L,
          0x0005a683L,
          0x00c5a023L,
          0x00d9a023L,
          0xfff48593L,
          0x000a0613L,
          0xfb1ff0efL,
          0x00048513L,
          0x05255663L,
          0x0009a583L,
          0x40a90633L,
          0x00251693L,
          0x014686b3L,
          0x00050493L,
          0x0100006fL,
          0xfff60613L,
          0x00468693L,
          0xfa060ae3L,
          0x0006a703L,
          0xfee5c8e3L,
          0x00249793L,
          0x014787b3L,
          0x0007a403L,
          0x00e7a023L,
          0x0086a023L,
          0x00148493L,
          0xfd5ff06fL,
          0x01c12083L,
          0x01812403L,
          0x01412483L,
          0x01012903L,
          0x00c12983L,
          0x00812a03L,
          0x02010113L,
          0x00008067L,
          // buffering a few instructions at the end
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
        )

        // if memory size is changed
        // change the first instruction of the
        // instruction sequence
        val memory = new Array[Byte](1024)

        for (i <- memory.indices) {
          memory(i) = 0.toByte
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

            println("%%%%%%% executed: " + instructionsExecuted + " %%%%%%%")

            instructionsExecuted += 1

            println("%%%%%%% executing inst_idx: " + inst_idx + ": 0x" + instructions(inst_idx).toHexString + " %%%%%%%")

            dut.io.iReadValue.valid.poke(true.B)
            dut.io.iReadValue.bits.poke(instructions(inst_idx).asUInt)
            dut.io.iReadValue.ready.expect(true.B)

            var readValue = 0L
            val readAddr = dut.io.dReadAddr.peek().litValue.toInt
            val readLen = dut.io.dReadLen.peek().litValue.toInt

            for (readOffset <- 0 until readLen) {
              var memValue = memory(readAddr + readOffset).toInt

              memValue = (memValue << 24) >>> 24

              readValue = readValue << 8
              readValue = readValue | memValue
            }

            printMem(memory)

            println(s"address: 0x${readAddr.toHexString}, len: $readLen")
            println(s"readValue: 0x${readValue.toHexString}")

            readValue = readValue << (64 - 8 * readLen)

            dut.io.dReadValue.poke(s"x${readValue.toHexString}".U)

            val writeAddr = dut.io.dWriteAddr.peek().litValue.toInt
            val writeLen = dut.io.dWriteLen.peek().litValue.toInt
            var writeData = dut.io.dWriteValue.peek().litValue.toLong

            println(s"writeData: 0x${writeData.toHexString}, writeAddr: 0x${writeAddr.toHexString}, writeLen: $writeLen")

            for (writeOffset <- 0 until writeLen) {
              memory(writeAddr + writeLen - writeOffset - 1) = (writeData & 0xFF).toByte
              writeData = writeData >>> 8
            }

            dut.clock.step()
          }
        }

        println(s"%%%%%%% completed, instructions executed: $instructionsExecuted %%%%%%%")
        println(memory.mkString("memory: (", ", ", ")"))

        printMem(memory)

        require(toInt(memory.slice(1016, 1020)) == 9)
        require(toInt(memory.slice(1012, 1016)) == 8)
        require(toInt(memory.slice(1008, 1012)) == 7)
        require(toInt(memory.slice(1004, 1008)) == 6)
        require(toInt(memory.slice(1000, 1004)) == 5)
        require(toInt(memory.slice(996,  1000)) == 4)
        require(toInt(memory.slice(992,   996)) == 3)
        require(toInt(memory.slice(988,   992)) == 2)
        require(toInt(memory.slice(984,   988)) == 1)
        require(toInt(memory.slice(980,   984)) == 0)
      }

      test(new InOrderPipelinedCPU(), Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        println("%%%%%%% Testing Quick Sort: RV64 %%%%%%%")

        val instructions = Seq(
          // set return address to out of bounds
          0x40000093L,
          // update stack size = 4096
          0x40000113L,
          0x40010113L,
          0x40010113L,
          0x40010113L,
          // actual instructions
          0xfa010113L,
          0x04113c23L,
          0x00100513L,
          0x00a13423L,
          0x00700513L,
          0x00a13823L,
          0x00600513L,
          0x00a13c23L,
          0x00500513L,
          0x02a13023L,
          0x00900513L,
          0x02a13423L,
          0x00400513L,
          0x02a13823L,
          0x02013c23L,
          0x00200513L,
          0x04a13023L,
          0x00300513L,
          0x04a13423L,
          0x00800513L,
          0x04a13823L,
          0x00900593L,
          0x00810613L,
          0x00000513L,
          0x010000efL,
          0x05813083L,
          0x06010113L,
          0x00008067L,
          0xfd010113L,
          0x02113423L,
          0x02813023L,
          0x00913c23L,
          0x01213823L,
          0x01313423L,
          0x00060993L,
          0x00058913L,
          0x00359413L,
          0x00c40433L,
          0x02c0006fL,
          0x00349593L,
          0x013585b3L,
          0x00043603L,
          0x0005b683L,
          0x00c5b023L,
          0x00d43023L,
          0xfff4859bL,
          0x00098613L,
          0xfb5ff0efL,
          0x00048513L,
          0x05255263L,
          0x00043583L,
          0x00351613L,
          0x01360633L,
          0x00050493L,
          0x00c0006fL,
          0x00860613L,
          0xfa860ee3L,
          0x00063683L,
          0xfed5cae3L,
          0x00349713L,
          0x01370733L,
          0x00073783L,
          0x00d73023L,
          0x00f63023L,
          0x0014849bL,
          0xfd9ff06fL,
          0x02813083L,
          0x02013403L,
          0x01813483L,
          0x01013903L,
          0x00813983L,
          0x03010113L,
          0x00008067L,
          // buffering a few instructions at the end
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
          0x00000033L, // ADD x0, x0, x0
        )

        // if memory size is changed
        // change the first instruction of the
        // instruction sequence
        val memory = new Array[Byte](4096)

        for (i <- memory.indices) {
          memory(i) = 0.toByte
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

            println("%%%%%%% executed: " + instructionsExecuted + " %%%%%%%")

            instructionsExecuted += 1

            println("%%%%%%% executing inst_idx: " + inst_idx + ": 0x" + instructions(inst_idx).toHexString + " %%%%%%%")

            dut.io.iReadValue.valid.poke(true.B)
            dut.io.iReadValue.bits.poke(instructions(inst_idx).asUInt)
            dut.io.iReadValue.ready.expect(true.B)

            var readValue = 0L
            val readAddr = dut.io.dReadAddr.peek().litValue.toInt
            val readLen = dut.io.dReadLen.peek().litValue.toInt

            println(s"address: 0x${readAddr.toHexString}, len: $readLen")

            for (readOffset <- 0 until readLen) {
              var memValue = memory(readAddr + readOffset).toInt

              memValue = (memValue << 24) >>> 24

              readValue = readValue << 8
              readValue = readValue | memValue
            }

            printMem(memory, 8, toLong)

            println(s"readValue: 0x${readValue.toHexString}")

            readValue = readValue << (64 - 8 * readLen)

            dut.io.dReadValue.poke(s"x${readValue.toHexString}".U)

            val writeAddr = dut.io.dWriteAddr.peek().litValue.toInt
            val writeLen = dut.io.dWriteLen.peek().litValue.toInt
            var writeData = dut.io.dWriteValue.peek().litValue.toLong

            println(s"writeData: 0x${writeData.toHexString}, writeAddr: 0x${writeAddr.toHexString}, writeLen: $writeLen")

            for (writeOffset <- 0 until writeLen) {
              memory(writeAddr + writeLen - writeOffset - 1) = (writeData & 0xFF).toByte
              writeData = writeData >>> 8
            }

            dut.clock.step()
          }
        }

        println(s"%%%%%%% completed, instructions executed: $instructionsExecuted %%%%%%%")
        println(memory.mkString("memory: (", ", ", ")"))

        printMem(memory, 8, toLong)

        require(toLong(memory.slice(4080, 4088)) == 9)
        require(toLong(memory.slice(4072, 4080)) == 8)
        require(toLong(memory.slice(4064, 4072)) == 7)
        require(toLong(memory.slice(4056, 4064)) == 6)
        require(toLong(memory.slice(4048, 4056)) == 5)
        require(toLong(memory.slice(4040, 4048)) == 4)
        require(toLong(memory.slice(4032, 4040)) == 3)
        require(toLong(memory.slice(4024, 4032)) == 2)
        require(toLong(memory.slice(4016, 4024)) == 1)
        require(toLong(memory.slice(4008, 4016)) == 0)
      }
    }

    {
      implicit val params = getParams(nPhyRegs = 8, nIQEntries = 4)

      test(new InstructionQueue()) { dut =>
        dut.io.wakeUpRegs.poke(0x0.U)

        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(1.U)
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(1.U)
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(3.U)
        dut.io.iqIdx.valid.expect(true.B)
        dut.clock.step()

        dut.io.instSignals.valid.poke(false.B)
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        dut.io.wakeUpRegs.poke(0b1010.U)
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        dut.io.wakeUpRegs.poke(0x0.U)

        dut.io.readyInstSignals.valid.expect(true.B)
        dut.io.readyInstSignals.bits.expect(1.U)
        dut.clock.step()

        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()
      }

      test(new InstructionQueue()) { dut =>
        println("---------------------------------------------")

        dut.io.wakeUpRegs.poke(0x0.U)

        // inst 0
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(0.U)
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(1.U)
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(7.U)
        dut.io.iqIdx.valid.expect(true.B)
        dut.clock.step()

        // inst 1
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(1.U)
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(2.U)
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(4.U)
        dut.io.iqIdx.valid.expect(true.B)
        dut.clock.step()

        // inst 2
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(2.U)
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(5.U)
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(6.U)
        dut.io.iqIdx.valid.expect(true.B)
        dut.clock.step()

        // waking up register 1
        dut.io.wakeUpRegs.poke(0b0000010.U)
        dut.io.instSignals.valid.poke(false.B)
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        // waking up register 2 & 4
        dut.io.wakeUpRegs.poke(0b0010100.U)
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        // inst 1 should've been woken up
        dut.io.wakeUpRegs.poke(0.U)
        dut.io.readyInstSignals.valid.expect(true.B)
        dut.io.readyInstSignals.bits.expect(1.U)
        dut.clock.step()

        // inst 3
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(3.U)
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(3.U)
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(6.U)
        dut.io.iqIdx.valid.expect(true.B)
        dut.clock.step()

        // waking up register 3, 5 & 6
        dut.io.instSignals.valid.poke(false.B)
        dut.io.wakeUpRegs.poke(0b1101000.U)
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        // inst 3 should've been woken up
        dut.io.readyInstSignals.valid.expect(true.B)
        dut.io.readyInstSignals.bits.expect(3.U)
        dut.clock.step()

        // inst 2 should've been woken up
        dut.io.readyInstSignals.valid.expect(true.B)
        dut.io.readyInstSignals.bits.expect(2.U)
        dut.clock.step()

        // wake up register 7 and then inst 0 should be ready
        dut.io.wakeUpRegs.poke(0b10000000.U)
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        dut.io.readyInstSignals.valid.expect(true.B)
        dut.io.readyInstSignals.bits.expect(0.U)
        dut.clock.step()

        dut.clock.step()
      }

      test(new InstructionQueue()) { dut =>
        println("---------------------------------------------")

        dut.io.wakeUpRegs.poke(0x0.U)

        // inst 0
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(0.U)
        // phy reg 1 -> 1
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(1.U)
        // phy reg 2 -> no deps
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(false.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(0.U)
        // check the allocation
        dut.io.iqIdx.valid.expect(true.B)
        // check the ready instructions
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        // inst 1
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(1.U)
        // phy reg 1 -> no dep
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(false.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(0.U)
        // phy reg 2 -> no dep
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(false.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(4.U)
        // check the allocation
        dut.io.iqIdx.valid.expect(true.B)
        // check the ready instructions
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        // inst 2
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(2.U)
        // phy reg 1 -> 5
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(5.U)
        // phy reg 2 -> 6
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(6.U)
        // check the allocation
        dut.io.iqIdx.valid.expect(true.B)

        // check the ready instructions
        dut.io.readyInstSignals.valid.expect(true.B)
        dut.io.readyInstSignals.bits.expect(1.U)
        dut.clock.step()

        // inst 3, same as inst 0
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(3.U)
        // phy reg 1 -> 1
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(1.U)
        // phy reg 2 -> no deps
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(false.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(0.U)
        // check the allocation
        dut.io.iqIdx.valid.expect(true.B)
        // check the ready instructions
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        // inst 4, same as inst 0
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(4.U)
        // phy reg 1 -> 1
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(1.U)
        // phy reg 2 -> no deps
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(false.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(0.U)
        // check the allocation
        dut.io.iqIdx.valid.expect(true.B)
        // check the ready instructions
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        // inst 5, same as inst 0, shouldn't allocate, IQ should be full
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.robIdx.poke(5.U)
        // phy reg 1 -> 1
        dut.io.instSignals.bits.rs1PhyReg.valid.poke(true.B)
        dut.io.instSignals.bits.rs1PhyReg.bits.poke(1.U)
        // phy reg 2 -> no deps
        dut.io.instSignals.bits.rs2PhyReg.valid.poke(false.B)
        dut.io.instSignals.bits.rs2PhyReg.bits.poke(0.U)
        // check the allocation
        dut.io.iqIdx.valid.expect(false.B)
        // check the ready instructions
        dut.io.readyInstSignals.valid.expect(false.B)
        dut.clock.step()

        dut.io.wakeUpRegs.poke(0b0000_0010.U)
        dut.io.instSignals.valid.poke(false.B)
        dut.clock.step()

        dut.io.wakeUpRegs.poke(0.U)
        dut.io.readyInstSignals.valid.expect(true.B)
        dut.io.readyInstSignals.bits.expect(0.U)
        dut.clock.step()

        dut.io.wakeUpRegs.poke(0b0110_0000.U)
        dut.io.readyInstSignals.valid.expect(true.B)
        dut.io.readyInstSignals.bits.expect(3.U)
        dut.clock.step()

        dut.io.wakeUpRegs.poke(0.U)
        dut.io.readyInstSignals.valid.expect(true.B)
        dut.io.readyInstSignals.bits.expect(2.U)
        dut.clock.step()

        dut.io.readyInstSignals.valid.expect(true.B)
        dut.io.readyInstSignals.bits.expect(4.U)
        dut.clock.step()

        dut.clock.step()
      }
    }

    {
      implicit val params = getParams(nROBEntries = 4)

      test(new ROB()) { dut =>
        dut.io.allocate.poke(true.B)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.expect(0.U)

        // inst 1
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.bits.poke(0x01.U)
        dut.io.robIdx.valid.expect(true.B)
        dut.io.robIdx.bits.expect(0.U)
        dut.clock.step()

        // inst 2
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.bits.poke(0x02.U)
        dut.io.robIdx.valid.expect(true.B)
        dut.io.robIdx.bits.expect(1.U)
        dut.clock.step()

        // inst 3
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.bits.poke(0x03.U)
        dut.io.robIdx.valid.expect(true.B)
        dut.io.robIdx.bits.expect(2.U)
        dut.clock.step()

        // inst 4 - won't fit in, addressing limitations
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.bits.poke(0x04.U)
        dut.io.robIdx.valid.expect(false.B)
        dut.clock.step()

        // commit inst 1
        dut.io.instSignals.valid.poke(false.B)
        dut.io.commitRobIdx.valid.poke(true.B)
        dut.io.commitRobIdx.bits.poke(0.U)
        dut.clock.step()

        // expect inst 1 to retire
        dut.io.retireInst.valid.expect(true.B)
        dut.io.retireInst.bits.signals.fetchSignals.instruction.valid.expect(true.B)
        dut.io.retireInst.bits.signals.fetchSignals.instruction.bits.expect(0x01.U)
        // commit inst 3
        dut.io.commitRobIdx.valid.poke(true.B)
        dut.io.commitRobIdx.bits.poke(2.U)
        dut.clock.step()

        // nothing should retire
        // BUT, should be able to allocate now
        // as inst 1 is retired now
        // inst 5
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.bits.poke(0x05.U)
        dut.io.robIdx.valid.expect(true.B)
        dut.io.robIdx.bits.expect(3.U)
        // ----------------------------------------
        dut.io.commitRobIdx.valid.poke(false.B)
        dut.io.retireInst.valid.expect(false.B)
        dut.clock.step()

        // still nothing should retire
        dut.io.instSignals.valid.poke(false.B)
        dut.io.commitRobIdx.valid.poke(false.B)
        // ----------------------------------------
        dut.io.retireInst.valid.expect(false.B)
        dut.clock.step()

        // commit instruction 2
        // still nothing should retire
        dut.io.commitRobIdx.valid.poke(true.B)
        dut.io.commitRobIdx.bits.poke(1.U)
        // ----------------------------------------
        dut.io.retireInst.valid.expect(false.B)
        dut.clock.step()

        // now, 2 & 3 should retire
        dut.io.commitRobIdx.valid.poke(false.B)
        // ----------------------------------------
        dut.io.retireInst.valid.expect(true.B)
        dut.io.retireInst.bits.signals.fetchSignals.instruction.valid.expect(true.B)
        dut.io.retireInst.bits.signals.fetchSignals.instruction.bits.expect(0x02.U)
        dut.clock.step()

        // inst 6
        dut.io.instSignals.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.valid.poke(true.B)
        dut.io.instSignals.bits.data.fetchSignals.instruction.bits.poke(0x06.U)
        dut.io.robIdx.valid.expect(true.B)
        dut.io.robIdx.bits.expect(0.U)
        // ----------------------------------------
        dut.io.retireInst.valid.expect(true.B)
        dut.io.retireInst.bits.signals.fetchSignals.instruction.valid.expect(true.B)
        dut.io.retireInst.bits.signals.fetchSignals.instruction.bits.expect(0x03.U)
        dut.clock.step()

        dut.io.instSignals.valid.poke(false.B)
        dut.clock.step()
      }
    }
    */

    {
      // address & data width
      val adW = 64
      // bit width
      val bw = 8
      // # bytes in data
      val db = adW / bw

      test(new Module {
        val io = IO(new Bundle {
          val in = Input(UInt(adW.W))
          val out = Output(UInt(adW.W))
        })

        io.out := MemoryO3.align(io.in, bw)
      }) { dut =>
        dut.io.in.poke(68.U)
        dut.io.out.expect(64.U)
      }

      test(new Module {
        val io = IO(new Bundle {
          val addr = Input(UInt(adW.W))
          val data = Input(UInt(adW.W))
          val rwSize = Input(MemRWSize())
          val dataOut = Output(UInt(adW.W))
        })

        io.dataOut := MemoryO3.readData(io.data, io.addr, io.rwSize, db, bw)
      }) { dut =>
        dut.io.data.poke(0x01020384050607L.U)

        dut.io.addr.poke(66.U)
        dut.io.rwSize.poke(MemRWSize.BYTES_2U)
        dut.io.dataOut.expect(0x8405.U)

        dut.io.addr.poke(67.U)
        dut.io.rwSize.poke(MemRWSize.BYTES_1U)
        dut.io.dataOut.expect(0x84.U)

        dut.io.addr.poke(64.U)
        dut.io.rwSize.poke(MemRWSize.BYTES_4U)
        dut.io.dataOut.expect(0x0000000084050607L.U)

        dut.io.addr.poke(64.U)
        dut.io.rwSize.poke(MemRWSize.BYTES_4S)
        dut.io.dataOut.expect("xFFFFFFFF84050607".U)

        dut.io.addr.poke(64.U)
        dut.io.rwSize.poke(MemRWSize.BYTES_8U)

        dut.io.dataOut.expect(0x01020384050607L.U)
      }

      test(new Module {
        val io = IO(new Bundle {
          val addr = Input(UInt(adW.W))
          val data = Input(UInt(adW.W))
          val rwSize = Input(MemRWSize())
          val dataOut = Output(UInt(adW.W))
          val maskOut = Output(UInt(db.W))
        })

        io.dataOut := MemoryO3.writeData(io.data, io.addr, io.rwSize, db, bw)
        io.maskOut := MemoryO3.mask(io.addr, io.rwSize, db)
      }) { dut =>
        dut.io.addr.poke(70.U)
        dut.io.data.poke(0x0607.U)
        dut.io.rwSize.poke(MemRWSize.BYTES_2S)

        dut.io.maskOut.expect(0b11_00_00_00.U)
        dut.io.dataOut.expect(0x0607_0000_0000_0000L.U)

        dut.io.addr.poke(64.U)
        dut.io.data.poke(0x0607.U)
        dut.io.rwSize.poke(MemRWSize.BYTES_8S)

        dut.io.maskOut.expect(0b11_11_11_11.U)
        dut.io.dataOut.expect(0x0000_0000_0000_0607L.U)

        dut.io.addr.poke(68.U)
        dut.io.data.poke(0x0607.U)
        dut.io.rwSize.poke(MemRWSize.BYTES_4S)

        dut.io.maskOut.expect(0b11_11_00_00.U)
        dut.io.dataOut.expect(0x0000_0607_0000_0000L.U)
      }
    }

    {
      implicit val params = getParams()

      test(new MemoryO3()) { dut =>
        dut.io.allocate.valid.poke(true.B)

        dut.io.allocate.bits.poke(MemRWDirection.read)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.idx.expect(0.U)

        dut.clock.step()

        dut.io.allocate.bits.poke(MemRWDirection.write)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.idx.expect(0.U)

        dut.clock.step()

        dut.io.allocate.bits.poke(MemRWDirection.read)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.idx.expect(1.U)

        dut.clock.step()

        dut.io.allocate.bits.poke(MemRWDirection.write)
        dut.io.allocatedIdx.valid.expect(true.B)
        dut.io.allocatedIdx.bits.idx.expect(1.U)

        dut.clock.step()

        dut.io.allocate.valid.poke(false.B)
        dut.io.allocatedIdx.valid.expect(false.B)
      }

      // todo: add more test cases maybe?
    }

    {
      implicit val params = getParams()

      test(new OutOfOrderCPU(), Seq(VerilatorBackendAnnotation)) { dut =>
        val instructions = Seq(
          0x00800293L, // ADDI x5, x0, 8
          0x00a28313L, // ADDI x6, x5, 10
          0x006303b3L, // ADD x7, x6, x6
          0x0073a023L, // SW x7, 0(x7)
        )

        dut.io.iReadAddr.valid.expect(true.B)
        dut.io.iReadAddr.bits.expect(0.U)
        dut.io.iReadAddr.ready.poke(true.B)

        dut.io.iReadValue.valid.poke(true.B)
        dut.io.iReadValue.bits.poke(instructions(0).asUInt)
        dut.io.iReadValue.ready.expect(true.B)

        dut.clock.step()

        dut.io.iReadAddr.valid.expect(true.B)
        dut.io.iReadAddr.bits.expect(4.U)

        dut.io.iReadValue.valid.poke(true.B)
        dut.io.iReadValue.bits.poke(instructions(1).asUInt)
        dut.io.iReadValue.ready.expect(true.B)
        dut.clock.step()

        dut.io.iReadAddr.valid.expect(true.B)
        dut.io.iReadAddr.bits.expect(8.U)

        dut.io.iReadValue.valid.poke(true.B)
        dut.io.iReadValue.bits.poke(instructions(2).asUInt)
        dut.io.iReadValue.ready.expect(true.B)
        dut.clock.step()

        dut.io.iReadAddr.valid.expect(true.B)
        dut.io.iReadAddr.bits.expect(12.U)

        dut.io.iReadValue.valid.poke(true.B)
        dut.io.iReadValue.bits.poke(instructions(3).asUInt)
        dut.io.iReadValue.ready.expect(true.B)
        dut.clock.step()

        dut.io.iReadAddr.valid.expect(true.B)
        dut.io.iReadAddr.bits.expect(16.U)

        dut.io.iReadValue.valid.poke(false.B)
        dut.io.iReadValue.bits.poke(0.U)

        breakable {
          while(true) {
            println("waiting...")

            if (dut.io.dMem.valid.peek().litToBoolean) {
              dut.io.dMem.bits.addr.expect(32.U)
              dut.io.dMem.bits.size.expect(3.U)
              dut.io.dMem.bits.write.valid.expect(true.B)
              dut.io.dMem.bits.write.bits.mask.expect(0b00001111.U)
              // high bits are supposed to be 0.
              // So, use integer instead.
              dut.io.dMem.bits.write.bits.value.expect(java.lang.Integer.reverse(36).U)
              break
            }

            dut.clock.step()
          }
        }
      }

      test(new OutOfOrderCPU(), Seq(VerilatorBackendAnnotation)) { dut =>
        val instructions = Seq(
          0x00800293L, // ADDI x5, x0, 8
          0x00000513L, // ADDI x10, x0, 0
          0x00550533L, // ADD x10, x10, x5
          0xfff28293L, // ADDI x5, x5, -1
          0xfe029ce3L, // BNE x5, x0, -8
          0x00a02023L, // SW x10, 0(x0)
        )

        println("O3 Loop testing")

        breakable {
          while(true) {
            dut.io.iReadAddr.valid.expect(true.B)
            dut.io.iReadAddr.ready.poke(true.B)

            val instIdx = dut.io.iReadAddr.bits.peek().litValue.intValue / 4

            println(s"Fetching instIdx: $instIdx")

            if (instIdx < instructions.length) {
              dut.io.iReadValue.valid.poke(true.B)
              dut.io.iReadValue.bits.poke(instructions(instIdx).asUInt)
              dut.io.iReadValue.ready.expect(true.B)
            } else {
              dut.io.iReadValue.valid.poke(false.B)
            }

            if (dut.io.dMem.valid.peek().litToBoolean) {
              dut.io.dMem.bits.addr.expect(0.U)
              dut.io.dMem.bits.size.expect(3.U)
              dut.io.dMem.bits.write.valid.expect(true.B)
              dut.io.dMem.bits.write.bits.mask.expect(0b11110000.U)
              dut.io.dMem.bits.write.bits.value.expect(java.lang.Long.reverse(36).U)
              break
            }

            dut.clock.step()
          }
        }
      }

      // todo: test load store forwarding and all
    }

    println("Compiling")

    implicit val params = getParams()

    val iqV = ChiselStage.emitSystemVerilog(
      gen = new OutOfOrderCPU(),
      firtoolOpts = Array("-disable-all-randomization")
    )

    val filePath = "/tmp/O3.v"

    Files.write(Paths.get(filePath), iqV.getBytes(StandardCharsets.UTF_8))
    println(s"File Updated: $filePath")
  }
}
