package io.riscy.stages

import chisel3.util.{Cat, Fill}
import chisel3.{Bundle, Input, Module, Mux, Output, PrintableHelper, UInt, fromBooleanToLiteral, fromIntToLiteral, fromIntToWidth, printf, when}
import io.riscy.stages.signals.Utils.NOP
import io.riscy.stages.signals.{DecodeSignals, Parameters}

class Decode()(implicit val params: Parameters) extends Module {
  val instWidth = params.instWidth
  val dataWidth = params.dataWidth

  assert(dataWidth >= 32)

  val io = IO(new Bundle {
    val inst = Input(UInt(instWidth.W))
    val signals = Output(DecodeSignals())
  })

  // the sign for all the instructions is always in the last bit
  val mask0 = Fill(dataWidth, 0.U(1.W))
  val mask1 = Fill(dataWidth, 1.U(1.W))
  val sign = Mux(io.inst(31) === 0.U, mask0, mask1)

  val immediateI = Cat(sign, io.inst(31, 20))(dataWidth - 1, 0)
  val immediateS = Cat(sign, io.inst(31, 25), io.inst(11, 7))(dataWidth - 1, 0)
  val immediateSB = Cat(sign, io.inst(31), io.inst(7), io.inst(30, 25), io.inst(11, 8), 0.U(1.W))(dataWidth - 1, 0)
  val immediateU = Cat(sign, io.inst(31, 12), 0.U(12.W))(dataWidth - 1, 0)
  val immediateUJ = Cat(sign, io.inst(31), io.inst(19, 12), io.inst(20), io.inst(30, 21), 0.U(1.W))(dataWidth - 1, 0)

  io.signals.jump := false.B
  io.signals.branch := false.B
  io.signals.memToReg := false.B
  io.signals.memRead := MemRWSize.BYTES_NO
  io.signals.memWrite := MemRWSize.BYTES_NO
  io.signals.rs1Pc := false.B
  io.signals.rs2Imm := false.B
  io.signals.regWrite := false.B
  io.signals.branchInvert := false.B
  io.signals.immediate := immediateI
  io.signals.word := false.B

  val rs1 = io.inst(19, 15)
  val rs2 = io.inst(24, 20)
  val rd = io.inst(11, 7)

  io.signals.rs1 := rs1
  io.signals.rs2 := rs2
  io.signals.rd := rd

  when(NOP.U === io.inst) {
    printf(cf"inst: NOP\n")

    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.ADD === io.inst) {
    printf(cf"inst: ADD\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.regWrite := true.B
  }.elsewhen(OpCode.ADDI === io.inst) {
    printf(cf"inst: ADDI\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.AND === io.inst) {
    printf(cf"inst: AND\n")

    io.signals.aluOp := ExecuteOp.AND
    io.signals.regWrite := true.B
  }.elsewhen(OpCode.ANDI === io.inst) {
    printf(cf"inst: ANDI\n")

    io.signals.aluOp := ExecuteOp.AND
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.AUIPC === io.inst) {
    printf(cf"inst: AUIPC\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.regWrite := true.B
    io.signals.rs1Pc := true.B
    io.signals.immediate := immediateU
  }.elsewhen(OpCode.BEQ === io.inst) {
    printf(cf"inst: BEQ\n")

    io.signals.aluOp := ExecuteOp.SUB
    io.signals.branch := true.B
    io.signals.immediate := immediateSB
  }.elsewhen(OpCode.BGE === io.inst) {
    printf(cf"inst: BGE\n")

    io.signals.aluOp := ExecuteOp.SLT
    io.signals.branch := true.B
    io.signals.branchInvert := true.B
    io.signals.immediate := immediateSB
  }.elsewhen(OpCode.BGEU === io.inst) {
    printf(cf"inst: BGEU\n")

    io.signals.aluOp := ExecuteOp.SLTU
    io.signals.branch := true.B
    io.signals.branchInvert := true.B
    io.signals.immediate := immediateSB
  }.elsewhen(OpCode.BLT === io.inst) {
    printf(cf"inst: BLT\n")

    io.signals.aluOp := ExecuteOp.SLT
    io.signals.branch := true.B
    io.signals.immediate := immediateSB
  }.elsewhen(OpCode.BLTU === io.inst) {
    printf(cf"inst: BLTU\n")

    io.signals.aluOp := ExecuteOp.SLTU
    io.signals.branch := true.B
    io.signals.immediate := immediateSB
  }.elsewhen(OpCode.BNE === io.inst) {
    printf(cf"inst: BNE\n")

    io.signals.aluOp := ExecuteOp.SUB
    io.signals.branch := true.B
    io.signals.branchInvert := true.B
    io.signals.immediate := immediateSB
  }.elsewhen(OpCode.JAL === io.inst) {
    // pc and imm are added in the ALU
    // rd is bypassed by checking the jump signal in the CPU
    // exec result is used to update the PC

    printf(cf"inst: JAL\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.jump := true.B
    io.signals.immediate := immediateUJ
    io.signals.regWrite := true.B
    io.signals.rs1Pc := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.JALR === io.inst) {
    printf(cf"inst: JALR\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.jump := true.B
    io.signals.immediate := immediateI
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.LB === io.inst) {
    printf(cf"inst: LB\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memRead := MemRWSize.BYTES_1S
    io.signals.memToReg := true.B
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.LBU === io.inst) {
    printf(cf"inst: LBU\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memRead := MemRWSize.BYTES_1U
    io.signals.memToReg := true.B
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.LH === io.inst) {
    printf(cf"inst: LH\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memRead := MemRWSize.BYTES_2S
    io.signals.memToReg := true.B
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.LHU === io.inst) {
    printf(cf"inst: LHU\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memRead := MemRWSize.BYTES_2U
    io.signals.memToReg := true.B
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.LUI === io.inst) {
    printf(cf"inst: LUI\n")

    io.signals.aluOp := ExecuteOp.FW_B
    io.signals.regWrite := true.B
    io.signals.immediate := immediateU
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.LW === io.inst) {
    printf(cf"inst: LW\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memRead := MemRWSize.BYTES_4S
    io.signals.memToReg := true.B
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.OR === io.inst) {
    printf(cf"inst: OR\n")

    io.signals.aluOp := ExecuteOp.OR
    io.signals.regWrite := true.B
  }.elsewhen(OpCode.ORI === io.inst) {
    printf(cf"inst: ORI\n")

    io.signals.aluOp := ExecuteOp.NOP
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.SB === io.inst) {
    printf(cf"inst: SB\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memWrite := MemRWSize.BYTES_1U
    io.signals.rs2Imm := true.B
    io.signals.immediate := immediateS
  }.elsewhen(OpCode.SH === io.inst) {
    printf(cf"inst: SH\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memWrite := MemRWSize.BYTES_2U
    io.signals.rs2Imm := true.B
    io.signals.immediate := immediateS
  }.elsewhen(OpCode.SLL === io.inst) {
    printf(cf"inst: SLL\n")

    io.signals.aluOp := ExecuteOp.SLL
    io.signals.regWrite := true.B
  }.elsewhen(OpCode.SLT === io.inst) {
    printf(cf"inst: SLT\n")

    io.signals.aluOp := ExecuteOp.SLT
    io.signals.regWrite := true.B
  }.elsewhen(OpCode.SLTI === io.inst) {
    printf(cf"inst: SLTI\n")

    io.signals.aluOp := ExecuteOp.SLT
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.SLTIU === io.inst) {
    printf(cf"inst: SLTIU\n")

    io.signals.aluOp := ExecuteOp.SLTU
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.SLTU === io.inst) {
    printf(cf"inst: SLTU\n")

    io.signals.aluOp := ExecuteOp.SLTU
    io.signals.regWrite := true.B
  }.elsewhen(OpCode.SRA === io.inst) {
    printf(cf"inst: SRA\n")

    io.signals.aluOp := ExecuteOp.SRA
    io.signals.regWrite := true.B
  }.elsewhen(OpCode.SRL === io.inst) {
    printf(cf"inst: SRL\n")

    io.signals.aluOp := ExecuteOp.SRL
    io.signals.regWrite := true.B
  }.elsewhen(OpCode.SUB === io.inst) {
    printf(cf"inst: SUB\n")

    io.signals.aluOp := ExecuteOp.SUB
    io.signals.regWrite := true.B
  }.elsewhen(OpCode.SW === io.inst) {
    printf(cf"inst: SW\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memWrite := MemRWSize.BYTES_4U
    io.signals.rs2Imm := true.B
    io.signals.immediate := immediateS
  }.elsewhen(OpCode.XOR === io.inst) {
    printf(cf"inst: XOR\n")

    io.signals.aluOp := ExecuteOp.XOR
    io.signals.regWrite := true.B
  }.elsewhen(OpCode.XORI === io.inst) {
    printf(cf"inst: XORI\n")

    io.signals.aluOp := ExecuteOp.XOR
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.EBREAK === io.inst) {
    // todo: begin
    printf(cf"inst: ** UnImplemented -> EBREAK **\n")

    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.ECALL === io.inst) {
    printf(cf"inst: ** UnImplemented -> ECALL **\n")

    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.FENCE === io.inst) {
    printf(cf"inst: ** UnImplemented -> FENCE **\n")

    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.FENCE_TSO === io.inst) {
    printf(cf"inst: ** UnImplemented -> FENCE_TSO **\n")

    io.signals.aluOp := ExecuteOp.NOP
  }.elsewhen(OpCode.PAUSE === io.inst) {
    printf(cf"inst: ** UnImplemented -> PAUSE **\n")

    io.signals.aluOp := ExecuteOp.NOP
    // todo: end
  }.elsewhen(OpCode.ADDIW === io.inst) {
    printf(cf"inst: ADDIW\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
    io.signals.word := true.B
  }.elsewhen(OpCode.ADDW === io.inst) {
    printf(cf"inst: ADDW\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.regWrite := true.B
    io.signals.word := true.B
  }.elsewhen(OpCode.LWU === io.inst) {
    printf(cf"inst: LWU\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memRead := MemRWSize.BYTES_4U
    io.signals.memToReg := true.B
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.SLLIW === io.inst) {
    printf(cf"inst: SLLIW\n")

    io.signals.aluOp := ExecuteOp.SLL
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
    io.signals.word := true.B
  }.elsewhen(OpCode.SLLW === io.inst) {
    printf(cf"inst: SLLW\n")

    io.signals.aluOp := ExecuteOp.SLL
    io.signals.regWrite := true.B
    io.signals.word := true.B
  }.elsewhen(OpCode.SRAIW === io.inst) {
    printf(cf"inst: SRAIW\n")

    io.signals.aluOp := ExecuteOp.SRA
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
    io.signals.word := true.B
  }.elsewhen(OpCode.SRAW === io.inst) {
    printf(cf"inst: SRAW\n")

    io.signals.aluOp := ExecuteOp.SRA
    io.signals.regWrite := true.B
    io.signals.word := true.B
  }.elsewhen(OpCode.SRLIW === io.inst) {
    printf(cf"inst: SRLIW\n")

    io.signals.aluOp := ExecuteOp.SRL
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
    io.signals.word := true.B
  }.elsewhen(OpCode.SRLW === io.inst) {
    printf(cf"inst: SRLW\n")

    io.signals.aluOp := ExecuteOp.SRL
    io.signals.regWrite := true.B
    io.signals.word := true.B
  }.elsewhen(OpCode.SUBW === io.inst) {
    printf(cf"inst: SUBW\n")

    io.signals.aluOp := ExecuteOp.SUB
    io.signals.regWrite := true.B
    io.signals.word := true.B
  }.elsewhen(OpCode.SLLI === io.inst) {
    printf(cf"inst: SLLI\n")

    io.signals.aluOp := ExecuteOp.SLL
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.SRLI === io.inst) {
    printf(cf"inst: SRLI\n")

    io.signals.aluOp := ExecuteOp.SRL
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.SRAI === io.inst) {
    printf(cf"inst: SRAI\n")

    io.signals.aluOp := ExecuteOp.SRA
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.LD === io.inst) {
    printf(cf"inst: LD\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memRead := MemRWSize.BYTES_8S
    io.signals.memToReg := true.B
    io.signals.regWrite := true.B
    io.signals.rs2Imm := true.B
  }.elsewhen(OpCode.SD === io.inst) {
    printf(cf"inst: SD\n")

    io.signals.aluOp := ExecuteOp.ADD
    io.signals.memWrite := MemRWSize.BYTES_8U
    io.signals.rs2Imm := true.B
    io.signals.immediate := immediateS
  }.otherwise {
    printf(cf"inst: ** Illegal OpCode: Inst: 0x${io.inst}%x **\n")

    io.signals.aluOp := ExecuteOp.UNKNOWN
  }

  chisel3.assert(io.signals.aluOp =/= ExecuteOp.UNKNOWN, cf"Illegal Op: ${io.inst}%x")
}
