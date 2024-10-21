package io.riscy.stages

import chisel3.util.{Cat, Decoupled, MuxLookup, PriorityMux, Reverse, Valid, isPow2, log2Ceil}
import chisel3.{Bool, Bundle, DontCare, Flipped, Input, Module, Mux, Output, PrintableHelper, Reg, RegInit, UInt, Vec, Wire, assert, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.MemoryO3.{align, isValid, mask, readData, resize, writeData}
import io.riscy.stages.signals.Parameters

// Contains data in the natural ordering for the hardware
// So, before sending out to the registers, need to correctly extend this
// and truncate it
class LoadQueueEntry()(implicit params: Parameters) extends Bundle {
  val reserved = Bool()
  val fired = Bool()
  val size = MemRWSize()
  val robIdx = Valid(UInt(log2Ceil(params.nROBEntries).W))
  val data = Valid(UInt(params.dataWidth.W))
  val mask = Valid(UInt((params.dataWidth / params.bitWidth).W))
  val address = Valid(UInt(params.addrWidth.W))
  val dstReg = Valid(UInt(log2Ceil(params.nPhyRegs).W))
}

// Contains data in the format that needs to be stored in the memory
// except for endianness
// Contains mask in the format that needed in the format of the data
// endianness mask(0) == true => data(7:0) is valid to be written
class StoreQueueEntry()(implicit params: Parameters) extends Bundle {
  val reserved = Bool()
  val retired = Bool()
  val fired = Bool()
  val size = MemRWSize()
  val data = Valid(UInt(params.dataWidth.W))
  val mask = Valid(UInt((params.dataWidth / params.bitWidth).W))
  val address = Valid(UInt(params.addrWidth.W))
}

class LoadStoreIndex()(implicit params: Parameters) extends Bundle {
  val rwDirection = MemRWDirection()
  val idx = UInt(LoadStoreIndex.width().W)

  def asLoadIndex(): UInt = idx(log2Ceil(params.nLDQEntries) - 1, 0)
  def asStoreIndex(): UInt = idx(log2Ceil(params.nSTQEntries) - 1, 0)
}

object LoadStoreIndex {
  def width()(implicit params: Parameters): Int = {
    math.max(log2Ceil(params.nLDQEntries), log2Ceil(params.nSTQEntries))
  }
}

class MemoryO3()(implicit params: Parameters) extends Module {
  val addrWidth = params.addrWidth
  val dataWidth = params.dataWidth
  val bitWidth = params.bitWidth

  val dataBytes = dataWidth / bitWidth

  val nPhyRegs = params.nPhyRegs
  val nLdQEntries = params.nLDQEntries
  val nStQEntries = params.nSTQEntries
  val nROBEntries = params.nROBEntries

  assert(dataWidth <= 8 * bitWidth)
  assert(isPow2(nLdQEntries))
  assert(isPow2(nStQEntries))

  val io = IO(new Bundle {
    // Processor interface
    val allocate = Input(Valid(MemRWDirection()))
    val allocatedIdx = Output(Valid(new LoadStoreIndex()))

    val retireIdx = Input(Valid(new LoadStoreIndex()))
    val flushIdx = Output(Valid(UInt(log2Ceil(nROBEntries).W)))

    val readData = Input(Valid(new Bundle {
      val ldIdx = UInt(log2Ceil(nLdQEntries).W)
      val dstReg = Valid(UInt(log2Ceil(nPhyRegs).W))
      val robIdx = UInt(log2Ceil(nROBEntries).W)
      val address = UInt(addrWidth.W)
      val size = Input(MemRWSize())
    }))

    val readException = Output(Valid(UInt(log2Ceil(nROBEntries).W)))

    val writeData = Input(Valid(new Bundle {
      val stIdx = UInt(log2Ceil(nStQEntries).W)
      val address = UInt(addrWidth.W)
      val size = Input(MemRWSize())
      val data = UInt(dataWidth.W)
    }))

    val readDataOut = Output(Valid(new Bundle {
      val robIdx = UInt(log2Ceil(nROBEntries).W)
      val dstReg = Valid(UInt(log2Ceil(nPhyRegs).W))
      val data = UInt(dataWidth.W)
    }))

    val flush = Input(Bool())

    // Memory interface
    val dMem = Decoupled(new Bundle {
      val addr = UInt(addrWidth.W)
      val size = UInt(log2Ceil(dataWidth / bitWidth).W)

      // If write is valid
      // Then operation is treated as write
      // Else
      // It's treated as read
      val write = Valid(new Bundle {
        val mask = UInt(dataBytes.W)
        val value = UInt(dataWidth.W)
      })
    })

    val dMemAck = Flipped(Decoupled(new Bundle {
      val size = UInt(log2Ceil(dataWidth / bitWidth).W)
      val value = UInt(dataWidth.W)
    }))
  })

  val requestInProgress = RegInit({
    val initSignals = Wire(Valid(new Bundle {
      val ignore = Bool()
      val idx = new LoadStoreIndex()

      // holding values
      val addr = UInt(addrWidth.W)
      val size = UInt(log2Ceil(dataWidth / bitWidth).W)
      val write = Bool()
      val writeMask = UInt(dataBytes.W)
      val writeValue = UInt(dataWidth.W)
    }))

    initSignals.valid := false.B
    initSignals.bits := DontCare

    initSignals
  })

  val loadQueue = Reg(Vec(nLdQEntries, new LoadQueueEntry()))

  val storeQueue = Reg(Vec(nStQEntries, new StoreQueueEntry()))

  val loadQueueHead = RegInit(0.U(log2Ceil(nLdQEntries).W))
  val loadQueueTail = RegInit(0.U(log2Ceil(nLdQEntries).W))

  val storeQueueHead = RegInit(0.U(log2Ceil(nStQEntries).W))
  val storeQueueTail = RegInit(0.U(log2Ceil(nStQEntries).W))
  val storeRetireTail = RegInit(0.U(log2Ceil(nStQEntries).W))

  val readOutIdx = RegInit({
    val initSignals = Wire(Valid(UInt(log2Ceil(nLdQEntries).W)))

    initSignals.valid := false.B
    initSignals.bits := DontCare

    initSignals
  })

  val ldCanAllocate = loadQueueHead =/= (loadQueueTail + 1.U)
  val stCanAllocate = storeQueueHead =/= (storeQueueTail + 1.U)

  val ldFireIdx = PriorityMux(Seq.tabulate(nLdQEntries + 1) { idx =>
    val idxW = Wire(Valid(UInt(log2Ceil(nLdQEntries).W)))

    if (idx < nLdQEntries) {
      val loadQueueLen = loadQueueTail - loadQueueHead
      val idxMatches = idx.U < loadQueueLen

      val correctedIdx = idx.U + loadQueueHead
      val ldEntry = loadQueue(correctedIdx)

      idxW.valid := true.B
      idxW.bits := correctedIdx

      (ldEntry.reserved && ldEntry.address.valid && !ldEntry.fired && idxMatches, idxW)
    } else {
      idxW.valid := false.B
      idxW.bits := DontCare

      (true.B, idxW)
    }
  })

  io.allocatedIdx.valid := false.B
  io.allocatedIdx.bits := DontCare

  io.flushIdx.valid := false.B
  io.flushIdx.bits := DontCare

  io.readException.valid := false.B
  io.readException.bits := DontCare

  when(io.allocate.valid) {
    printf(cf"Memory: trying to allocate, allocate: ${io.allocate} ldCanAllocate: $ldCanAllocate, stCanAllocate: $stCanAllocate\n")
    printf(cf"Memory: loadQueueHead: $loadQueueHead tail: $loadQueueTail\n")
    printf(cf"Memory: storeQueueHead: $storeQueueHead retireTail: $storeRetireTail tail: $storeQueueTail\n")

    when(io.allocate.bits === MemRWDirection.read) {
      io.allocatedIdx.valid := ldCanAllocate
      io.allocatedIdx.bits.rwDirection := MemRWDirection.read
      io.allocatedIdx.bits.idx := loadQueueTail

      when(ldCanAllocate) {
        loadQueue(loadQueueTail).reserved := true.B
        loadQueue(loadQueueTail).address.valid := false.B
        loadQueue(loadQueueTail).fired := false.B
        loadQueue(loadQueueTail).data.valid := false.B

        loadQueueTail := loadQueueTail + 1.U
      }
    }.elsewhen(io.allocate.bits === MemRWDirection.write) {
      io.allocatedIdx.valid := stCanAllocate
      io.allocatedIdx.bits.rwDirection := MemRWDirection.write
      io.allocatedIdx.bits.idx := storeQueueTail

      when(stCanAllocate) {
        storeQueue(storeQueueTail).reserved := true.B
        storeQueue(storeQueueTail).retired := false.B
        storeQueue(storeQueueTail).fired := false.B

        storeQueueTail := storeQueueTail + 1.U
      }
    }.otherwise {
      assert(false.B, "Illegal rw direction, not handled in code")
    }
  }

  when(io.readData.valid) {
    val ldIdx = io.readData.bits.ldIdx
    val size = io.readData.bits.size
    val address = io.readData.bits.address
    val dstReg = io.readData.bits.dstReg
    val robIdx = io.readData.bits.robIdx

    printf(cf"Memory: ldIdx: $ldIdx size: $size address: $address dstReg: $dstReg robIdx: $robIdx\n")

    when(isValid(address, size, dataBytes)) {
      loadQueue(ldIdx).size := size
      loadQueue(ldIdx).address.valid := true.B
      loadQueue(ldIdx).address.bits := address
      loadQueue(ldIdx).dstReg := dstReg
      loadQueue(ldIdx).robIdx.valid := true.B
      loadQueue(ldIdx).robIdx.bits := robIdx
      loadQueue(ldIdx).mask.valid := true.B
      loadQueue(ldIdx).mask.bits := mask(address, size, dataBytes)
    }.otherwise {
      io.readException.valid := true.B
      io.readException.bits := robIdx
    }
  }

  when(io.writeData.valid) {
    val stIdx = io.writeData.bits.stIdx

    val size = io.writeData.bits.size
    val address = io.writeData.bits.address
    val data = io.writeData.bits.data

    printf(cf"Memory: stIdx: $stIdx size: $size address: $address\n")

    storeQueue(stIdx).size := size
    storeQueue(stIdx).address.valid := true.B
    storeQueue(stIdx).address.bits := address
    storeQueue(stIdx).data.valid := true.B
    storeQueue(stIdx).data.bits := writeData(data, address, size, dataBytes, bitWidth)
    storeQueue(stIdx).mask.valid := true.B
    storeQueue(stIdx).mask.bits := mask(address, size, dataBytes)
  }

  when(io.retireIdx.valid) {
    printf(cf"Memory: Retiring: ${io.retireIdx}\n")

    // NOTE:
    // When a store is retired, it has to check
    // the loadQueue to check if any loads has loaded the same address
    // If they did, and it's younger (which is always the case)
    // the oldest of those loads and the following instructions must be flushed
    // Similarly,
    // When a load is invoked, it has to check the store buffer for all
    // the stores storing to the same location and try to load from them

    // NOTE:
    // load entries are deallocated as soon as they are retired
    // store entries are deallocated as they are issued to the memory

    when(io.retireIdx.bits.rwDirection === MemRWDirection.read) {
      assert(io.retireIdx.bits.asLoadIndex() === loadQueueHead, cf"loadQueueHead: $loadQueueHead retireIdx: ${io.retireIdx.bits.asLoadIndex()}\n")

      loadQueue(io.retireIdx.bits.asLoadIndex()).reserved := false.B
      loadQueueHead := loadQueueHead + 1.U
    }

    when(io.retireIdx.bits.rwDirection === MemRWDirection.write) {
      val stIdx = io.retireIdx.bits.asStoreIndex()
      val stAddr = storeQueue(stIdx).address.bits

      assert(storeQueue(stIdx).address.valid)
      assert(storeRetireTail === stIdx, cf"retire $stIdx != retire tail $storeRetireTail\n")

      printf(cf"Memory: retiring: $stIdx\n")

      storeQueue(stIdx).retired := true.B
      storeRetireTail := storeRetireTail + 1.U

      printf(cf"Memory: loadQueueTail: $loadQueueTail loadQueueHead: $loadQueueHead\n")

      val flushIdx = PriorityMux(Seq.tabulate(nLdQEntries + 1) { idx =>
        val flushIdxW = Wire(Valid(UInt(log2Ceil(nROBEntries).W)))

        if (idx < nLdQEntries) {
          val ldIdx = loadQueueHead + idx.U
          val loadQueueLen = loadQueueTail - loadQueueHead

          val valid = idx.U < loadQueueLen && loadQueue(ldIdx).address.valid && align(loadQueue(ldIdx).address.bits, dataBytes) === align(stAddr, dataBytes)

          val robIdx = loadQueue(ldIdx).robIdx.bits

          flushIdxW.valid := valid
          flushIdxW.bits := robIdx

          // printf(cf"Memory: ldIdx: $ldIdx, valid: $valid robIdx: $robIdx\n")

          (valid, flushIdxW)
        } else {
          assert(idx == nLdQEntries)

          flushIdxW.valid := false.B
          flushIdxW.bits := DontCare

          (true.B, flushIdxW)
        }
      })

      io.flushIdx := flushIdx
    }
  }

  // Always prioritizing stores over loads
  // This might starve the loads, but, as the stores
  // are only issued in the program order, this makes sense
  val stFireIdx = Wire(Valid(UInt(log2Ceil(nStQEntries).W)))
  val stHead = storeQueue(storeQueueHead)

  stFireIdx.valid := stHead.reserved && stHead.retired && !stHead.fired
  stFireIdx.bits := storeQueueHead

  when(stFireIdx.valid && !requestInProgress.valid) {
    val stEntry = storeQueue(stFireIdx.bits)

    assert(stEntry.address.valid)
    assert(stEntry.mask.valid)
    assert(stEntry.data.valid)

    val addr = align(stEntry.address.bits, dataBytes)
    val size = log2Ceil(dataBytes).U
    val mask = Reverse(stEntry.mask.bits)
    val value = Reverse(stEntry.data.bits)

    io.dMem.valid := true.B
    io.dMem.bits.addr := addr
    io.dMem.bits.size := size

    // Endianness is quintessential
    // Store long to address 0
    // Load int from address 0
    // MUST always lead to reading the same bits being read

    io.dMem.bits.write.valid := true.B

    // Reversing to main memory here
    io.dMem.bits.write.bits.mask := mask
    io.dMem.bits.write.bits.value := value

    when(io.dMem.ready) {
      storeQueue(stFireIdx.bits).fired := true.B

      requestInProgress.valid := true.B
      requestInProgress.bits.ignore := false.B
      requestInProgress.bits.idx.rwDirection := MemRWDirection.write
      requestInProgress.bits.idx.idx := resize(stFireIdx.bits, LoadStoreIndex.width())

      requestInProgress.bits.addr := addr
      requestInProgress.bits.size := size
      requestInProgress.bits.write := true.B
      requestInProgress.bits.writeMask := mask
      requestInProgress.bits.writeValue := value
    }

    printf(cf"Memory: store fired entry: ${stFireIdx.bits} => $stEntry\n")
  }.elsewhen(ldFireIdx.valid && !requestInProgress.valid) {
    val ldEntry = loadQueue(ldFireIdx.bits)

    assert(ldEntry.address.valid)

    val addr = align(ldEntry.address.bits, dataBytes)
    val size = log2Ceil(dataBytes).U

    io.dMem.valid := true.B
    io.dMem.bits.addr := addr
    io.dMem.bits.size := size

    io.dMem.bits.write.valid := false.B
    io.dMem.bits.write.bits := DontCare

    when(io.dMem.ready) {
      loadQueue(ldFireIdx.bits).fired := true.B

      requestInProgress.valid := true.B
      requestInProgress.bits.ignore := false.B
      requestInProgress.bits.idx.rwDirection := MemRWDirection.read
      requestInProgress.bits.idx.idx := resize(ldFireIdx.bits, LoadStoreIndex.width())

      requestInProgress.bits.addr := addr
      requestInProgress.bits.size := size
      requestInProgress.bits.write := false.B
      requestInProgress.bits.writeMask := DontCare
      requestInProgress.bits.writeValue := DontCare
    }

    printf(cf"Memory: load fired entry: ${ldFireIdx.bits} => $ldEntry\n")
  }.otherwise {
    io.dMem.valid := false.B
    io.dMem.bits := DontCare

    printf(cf"Memory: no memory transaction this cycle\n")
  }

  // hold the values when the request is in progress
  when(requestInProgress.valid) {
    io.dMem.valid := true.B
    io.dMem.bits.addr := requestInProgress.bits.addr
    io.dMem.bits.size := requestInProgress.bits.size
    io.dMem.bits.write.valid := requestInProgress.bits.write
    io.dMem.bits.write.bits.mask := requestInProgress.bits.writeMask
    io.dMem.bits.write.bits.value := requestInProgress.bits.writeValue
  }

  when(io.dMemAck.valid) {
    val ackData = io.dMemAck.deq()

    printf(cf"Memory: ack: $ackData\n")

    assert(requestInProgress.valid)

    assert(io.dMemAck.bits.size === log2Ceil(dataBytes).U)

    when(requestInProgress.bits.idx.rwDirection === MemRWDirection.read) {
      val storeQueueLen = storeQueueTail - storeQueueHead
      val ldIdx = requestInProgress.bits.idx.asLoadIndex()

      printf(cf"Memory: loaded: ldIdx: $ldIdx = ${ackData.value}\n")

      val value = Wire(Vec(dataWidth, Bool()))

      // Reversing from main memory here
      value := Reverse(ackData.value).asBools

      when(!requestInProgress.bits.ignore) {
        assert(loadQueue(ldIdx).address.valid)
        assert(loadQueue(ldIdx).mask.valid)

        for (off <- 0 until nStQEntries) {
          val idx = storeQueueHead + off.U

          val stEntry = storeQueue(idx)

          when(off.U < storeQueueLen && stEntry.reserved && stEntry.retired) {
            assert(stEntry.address.valid)
            assert(stEntry.data.valid)
            assert(stEntry.mask.valid)

            val ldMask = loadQueue(ldIdx).mask.bits
            val ldAddr = align(loadQueue(ldIdx).address.bits, dataBytes)

            val stMask = stEntry.mask.bits
            val stAddr = align(stEntry.address.bits, dataBytes)

            when(ldAddr === stAddr && (ldMask & stMask) =/= 0.U) {
              val m = stEntry.mask.bits
              val d = stEntry.data.bits

              for (w <- 0 until dataBytes) {
                when(m(w)) {
                  for (bitIdx <- 0 until bitWidth) {
                    value(w * bitWidth + bitIdx) := d(w * bitWidth + bitIdx)
                  }
                }
              }
            }
          }
        }

        loadQueue(ldIdx).data.valid := true.B
        loadQueue(ldIdx).data.bits := value.asUInt

        readOutIdx.valid := true.B
        readOutIdx.bits := ldIdx
      }
    }

    when(requestInProgress.bits.idx.rwDirection === MemRWDirection.write) {
      assert(storeQueueHead === requestInProgress.bits.idx.asStoreIndex(), cf"storeQueueHead: $storeQueueHead, rip: $requestInProgress")

      storeQueue(requestInProgress.bits.idx.asStoreIndex()).reserved := false.B
      storeQueueHead := storeQueueHead + 1.U
    }

    requestInProgress.valid := false.B
  }.otherwise {
    io.dMemAck.nodeq()
  }

  when(readOutIdx.valid) {
    val ldIdx = readOutIdx.bits
    val data = loadQueue(ldIdx).data.bits
    val addr = loadQueue(ldIdx).address.bits
    val size = loadQueue(ldIdx).size

    val readValue = readData(data, addr, size, dataBytes, bitWidth)

    printf(cf"Memory: read ldIdx: $ldIdx @ $addr($size) = 0x$readValue%x\n")

    readOutIdx.valid := false.B

    assert(loadQueue(readOutIdx.bits).data.valid)
    assert(loadQueue(readOutIdx.bits).robIdx.valid)

    io.readDataOut.valid := true.B
    io.readDataOut.bits.dstReg := loadQueue(readOutIdx.bits).dstReg

    io.readDataOut.bits.robIdx := loadQueue(readOutIdx.bits).robIdx.bits

    io.readDataOut.bits.data := readValue
  }.otherwise {
    io.readDataOut.valid := false.B
    io.readDataOut.bits := DontCare
  }

  when(io.flush) {
    loadQueueHead := 0.U
    loadQueueTail := 0.U

    storeQueueTail := storeRetireTail

    requestInProgress.bits.ignore := true.B

    readOutIdx.valid := false.B
    readOutIdx.bits := DontCare
  }
}

object MemoryO3 {
  def isValid(addr: UInt, rwSize: MemRWSize.Type, dataBytes: Int): Bool = {
    val size = Memory.getSizeBytes(rwSize)
    val off = offset(addr, dataBytes)

    off +& size <= dataBytes.U
  }

  def align(addr: UInt, dataBytes: Int): UInt = {
    require(isPow2(dataBytes))

    val dataBytesLog = log2Ceil(dataBytes)

    ((addr >> dataBytesLog) << dataBytesLog).asUInt
  }

  def offset(addr: UInt, dataBytes: Int): UInt = {
    require(isPow2(dataBytes))

    val dataBytesLog = log2Ceil(dataBytes)

    addr(dataBytesLog - 1, 0)
  }

  def readData(data: UInt, addr: UInt, rwSize: MemRWSize.Type, dataBytes: Int, bitWidth: Int): UInt = {
    val off = offset(addr, dataBytes)
    val size = Memory.getSizeBytes(rwSize)

    assert(off < dataBytes.U)
    assert(off +& size <= dataBytes.U)

    val bitWidthLog = log2Ceil(bitWidth)

    val bitOffset = (off << bitWidthLog).asUInt
    val dataAfterOffset = (data >> bitOffset).asUInt

    MuxLookup(rwSize, 0.U)(
      MemRWSize.all
        .filter { s => s != MemRWSize.BYTES_NO }
        .map { s =>
          val sizeLit = Memory.getSizeBytesLit(s)
          val isSigned = Memory.isSigned(s)

          val zExt = 0.U(((dataBytes - sizeLit) * bitWidth).W)
          val oExt = ~zExt

          val sign = dataAfterOffset(sizeLit * bitWidth - 1)

          val ext = Mux(isSigned && sign, oExt, zExt)

          s -> Cat(ext, dataAfterOffset(sizeLit * bitWidth - 1, 0))
        })
  }

  def writeData(data: UInt, addr: UInt, rwSize: MemRWSize.Type, dataBytes: Int, bitWidth: Int): UInt = {
    val off = offset(addr, dataBytes)
    val size = Memory.getSizeBytes(rwSize)
    val dataWidth = dataBytes * bitWidth

    val bitWidthLog = log2Ceil(bitWidth)

    assert(off < dataBytes.U)
    assert(off +& size <= dataBytes.U)

    val outData = Cat(Seq.tabulate(dataWidth) { rIdx => {
      val idx = (dataWidth - 1) - rIdx

      val byteIdx = idx / bitWidth
      val bitIdx = idx % bitWidth

      val out = Wire(Bool())

      when(off <= byteIdx.U && byteIdx.U < off +& size) {
        val byteOffset = byteIdx.U - off

        out := data((byteOffset << bitWidthLog).asUInt + bitIdx.U)
      }.otherwise {
        out := 0.U(1.W)
      }

      out
    }})

    outData
  }

  def mask(addr: UInt, rwSize: MemRWSize.Type, dataBytes: Int): UInt = {
    val off = offset(addr, dataBytes)
    val size = Memory.getSizeBytes(rwSize)

    assert(off +& size <= dataBytes.U)

    val mask = Wire(UInt(dataBytes.W))

    // Cat operates left to right
    // But data bits are in right to left, so, reverse it
    // So, reverse the bits ¯\_(ツ)_/¯
    mask := Reverse(Cat(Seq.tabulate(dataBytes) { idx => Mux(off <= idx.U && idx.U < off +& size, 1.U(1.W), 0.U(1.W)) }))

    mask
  }

  def resize(in: UInt, width: Int): UInt = {
    val inWidth = in.getWidth

    if (inWidth >= width) {
      in(width - 1, 0)
    } else {
      Cat(0.U((width - inWidth).W), in)
    }
  }
}
