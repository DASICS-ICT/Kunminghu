// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.decode.{DecodeUnit, ImmUnion}
import xiangshan.backend.issue.ImmExtractor

class FDICallDecodeHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val instruction = Input(UInt(32.W))
    val ftqPC = Input(UInt(64.W))
    val slot = Input(UInt(log2Up(PredictWidth).W))
    val oldRs1 = Input(UInt(64.W))
    val isRVC = Input(Bool())
    val illegal = Output(Bool())
    val selector = Output(SelImm())
    val minBits = Output(UInt(ImmUnion.maxLen.W))
    val offset = Output(UInt(64.W))
    val rd = Output(UInt(5.W))
    val rs1 = Output(UInt(5.W))
    val rs2 = Output(UInt(5.W))
    val srcType = Output(Vec(3, UInt(4.W)))
    val writeGPR = Output(Bool())
    val jump = Output(Bool())
    val func = Output(FuOpType())
    val waitForward = Output(Bool())
    val blockBackward = Output(Bool())
    val result = Output(UInt(64.W))
    val target = Output(UInt(64.W))
    val auipc = Output(Bool())
    val rawSelector = Input(SelImm())
    val rawMinBits = Input(UInt(32.W))
    val rawImmediate = Output(UInt(64.W))
    val decodedImmediate = Output(UInt(64.W))
  })

  require(SelImm().getWidth == 5)
  require(ImmUnion.maxLen == 22)
  require(FuConfig.JmpCfg.immType.map(SelImm.getImmUnion(_).len).max == 22)
  require(FuConfig.JmpCfg.immType.map(_.litValue) == Set(BigInt(2), BigInt(3), BigInt(4), BigInt(16)))
  private val jumpQueues = backendParams.allIssueParams.filter(_.getFuCfgs.contains(FuConfig.JmpCfg))
  require(jumpQueues.nonEmpty)
  require(jumpQueues.forall(_.deqImmTypesMaxLen >= 22))
  val decoder = Module(new DecodeUnit)
  decoder.io.enq.ctrlFlow := 0.U.asTypeOf(decoder.io.enq.ctrlFlow)
  decoder.io.enq.ctrlFlow.instr := io.instruction
  decoder.io.enq.ctrlFlow.ftqOffset := io.slot
  decoder.io.enq.ctrlFlow.preDecodeInfo.isRVC := io.isRVC
  decoder.io.enq.vtype := 0.U.asTypeOf(decoder.io.enq.vtype)
  decoder.io.enq.vstart := 0.U
  decoder.io.csrCtrl := 0.U.asTypeOf(decoder.io.csrCtrl)
  decoder.io.fromCSR := 0.U.asTypeOf(decoder.io.fromCSR)
  val decoded = decoder.io.deq.decodedInst
  val offset = ImmExtractor(decoded.imm, decoded.selImm, 64,
    FuConfig.JmpCfg.immType.map(_.litValue))

  val jump = Module(new JumpDataModule)
  jump.io.src := io.oldRs1
  jump.io.pc := io.ftqPC
  // This adapter exercises the unchanged FTQ-based consumer contract, not a ROB completion.
  jump.io.imm := offset + Mux(JumpOpType.jumpOpisJalr(decoded.fuOpType),
    0.U, io.slot << instOffsetBits)
  jump.io.nextPcOffset := io.slot +& Mux(io.isRVC, 1.U, 2.U)
  jump.io.func := decoded.fuOpType
  jump.io.isRVC := io.isRVC
  io.result := jump.io.result
  io.target := jump.io.target
  io.auipc := jump.io.isAuipc
  io.illegal := decoded.exceptionVec(ExceptionNO.illegalInstr)
  io.selector := decoded.selImm
  io.minBits := decoded.imm
  io.offset := offset
  io.rd := decoded.ldest
  io.rs1 := decoded.lsrc(0)
  io.rs2 := decoded.lsrc(1)
  io.srcType := VecInit(decoded.srcType.take(3))
  io.writeGPR := decoded.rfWen
  io.jump := FuType.FuTypeOrR(decoded.fuType, FuType.jmp)
  io.func := decoded.fuOpType
  io.waitForward := decoded.waitForward
  io.blockBackward := decoded.blockBackward

  private val selectors = (ImmUnion.immSelMap.map(_._1) :+ SelImm.IMM_LUI32).map(_.litValue).toSet
  io.rawImmediate := ImmExtractor(io.rawMinBits, io.rawSelector, 64, selectors)
  io.decodedImmediate := ImmExtractor(decoded.imm, decoded.selImm, 64, selectors)
}

class FDICallDecodeTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "DASICS call decode and arithmetic"
  private val mask64 = (BigInt(1) << 64) - 1
  private def signed(value: BigInt, width: Int): BigInt =
    if (value.testBit(width - 1)) value - (BigInt(1) << width) else value
  private def wordJOffset(word: BigInt): BigInt = {
    val bits = ((word >> 31) & 1) << 22 |
      ((word >> 7) & 1) << 21 | ((word >> 15) & 63) << 15 |
      ((word >> 8) & 15) << 11 | ((word >> 21) & 1023) << 1
    signed(bits, 23)
  }

  for (enabled <- Seq(false, true)) {
    it should s"preserve instruction fields and FTQ-relative arithmetic with HasFDI=$enabled" in {
      val runRoot = java.nio.file.Paths.get(sys.props("f01.runRoot")).toRealPath()
      assert(java.nio.file.Paths.get("").toRealPath() == runRoot)
      val base = new top.DefaultConfig
      implicit val p: Parameters = base.alterPartial {
        case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
        case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
          EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
          EnableChiselDB = false, AlwaysBasicDB = false)
      }
      test(new FDICallDecodeHarness).withAnnotations(Seq(VerilatorBackendAnnotation,
        TargetDirAnnotation(s"decode-${if (enabled) "on" else "off"}"))) { dut =>
        dut.io.rawSelector.poke(4.U)
        dut.io.rawMinBits.poke(0.U)
        dut.io.isRVC.poke(false.B)
        dut.io.slot.poke(0.U)
        dut.io.ftqPC.poke(BigInt("80000000", 16).U)
        dut.io.oldRs1.poke(BigInt("80010001", 16).U)
        var cases = 0
        val fixedJ = Seq(
          ("0000000b", BigInt(0)), ("0020000b", BigInt(2)),
          ("ffff8f8b", BigInt(-2)), ("8000000b", BigInt(-4194304)),
          ("7fff8f8b", BigInt(4194302)))
        val random = new scala.util.Random(0x4644494aL)
        val randomWords = (0 until 96).map { _ =>
          (BigInt(32, random) & BigInt("ffff8f80", 16)) | BigInt(0x0b)
        }
        // Literal instruction bit positions cover every minBits bit independently.
        val immediateInstructionBits = (21 to 30) ++ (8 to 11) ++ (15 to 20) ++ Seq(7, 31)
        val walkingWords = immediateInstructionBits.zipWithIndex.flatMap { case (position, minimumBit) =>
          val word = (BigInt(1) << position) | BigInt(0x0b)
          val expected = if (minimumBit == 21) -BigInt(4194304) else BigInt(1) << (minimumBit + 1)
          assert(wordJOffset(word) == expected)
          Seq(word, (BigInt("ffff8f80", 16) ^ (BigInt(1) << position)) | BigInt(0x0b))
        }
        val words = fixedJ.map { case (word, _) => BigInt(word, 16) } ++ walkingWords ++ randomWords
        for ((text, value) <- fixedJ) assert(wordJOffset(BigInt(text, 16)) == value)
        for (word <- words; slot <- Seq(0, 3); pc <- Seq(BigInt("80000000", 16), mask64 - 7)) {
          val offset = wordJOffset(word)
          dut.io.instruction.poke(word.U)
          dut.io.slot.poke(slot.U)
          dut.io.ftqPC.poke(pc.U)
          dut.clock.step()
          // Architectural call completion is separately authorized by the execution path.
          dut.io.illegal.expect((!enabled).B)
          if (enabled) {
            dut.io.jump.expect(true.B)
            dut.io.selector.expect(16.U)
            dut.io.func.expect(4.U)
            dut.io.rd.expect(1.U)
            dut.io.rs1.expect(((word >> 15) & 31).U)
            dut.io.rs2.expect(((word >> 20) & 31).U)
            // Numeric types are independent expectations: PC/immediate=0, integer register=1.
            dut.io.srcType(0).expect(0.U)
            dut.io.srcType(1).expect(0.U)
            dut.io.srcType(2).expect(0.U)
            dut.io.writeGPR.expect(true.B)
            dut.io.minBits.expect(((offset & ((BigInt(1) << 23) - 1)) >> 1).U)
            dut.io.offset.expect((offset & mask64).U)
            dut.io.target.expect(((pc + slot * 2 + offset) & mask64).U)
            dut.io.result.expect(((pc + slot * 2 + 4) & mask64).U)
            dut.io.auipc.expect(false.B)
            dut.io.waitForward.expect(true.B)
            dut.io.blockBackward.expect(true.B)
          }
          cases += 1
        }
        for (rd <- Seq(0, 1, 31); rs1 <- Seq(0, 1, 31); immediate <- Seq(-2048, -1, 0, 1, 2047)) {
          val word = (BigInt(immediate & 0xfff) << 20) | (BigInt(rs1) << 15) |
            (BigInt(rd) << 7) | BigInt(0x100b)
          val oldValue = if (rs1 == 0) BigInt(0) else mask64 - 31
          dut.io.instruction.poke(word.U)
          dut.io.oldRs1.poke(oldValue.U)
          dut.io.slot.poke(3.U)
          dut.io.ftqPC.poke(BigInt("80000000", 16).U)
          dut.clock.step()
          dut.io.illegal.expect((!enabled).B)
          if (enabled) {
            dut.io.selector.expect(4.U)
            dut.io.func.expect(5.U)
            dut.io.rd.expect(rd.U)
            dut.io.rs1.expect(rs1.U)
            dut.io.rs2.expect(((word >> 20) & 31).U)
            dut.io.srcType(0).expect(1.U)
            dut.io.srcType(1).expect(0.U)
            dut.io.srcType(2).expect(0.U)
            dut.io.writeGPR.expect((rd != 0).B)
            dut.io.minBits.expect((immediate & 0xfff).U)
            dut.io.offset.expect((BigInt(immediate) & mask64).U)
            dut.io.target.expect(((oldValue + immediate) & (mask64 ^ 1)).U)
            dut.io.result.expect(BigInt("8000000a", 16).U)
          }
          cases += 1
        }
        for (function <- 2 to 7) {
          dut.io.instruction.poke(((function << 12) | 0x0b).U)
          dut.clock.step()
          dut.io.illegal.expect(true.B)
          cases += 1
        }

        val oldImmediates = Seq(
          (4, BigInt(0x800), BigInt(-2048)), (14, BigInt(0xfff), BigInt(-1)),
          (1, BigInt(0x800), BigInt(-4096)), (2, BigInt(0x80000), BigInt(-2147483648L)),
          (3, BigInt(0x80000), BigInt(-1048576)),
          // Packed CSR fields remain 22 bits and the issue extractor sign-extends bit 21.
          (5, BigInt(0x3fffff), BigInt(-1)),
          (5, BigInt(0x200000), BigInt(-2097152)), (5, BigInt(0x1fffff), BigInt(2097151)),
          (8, BigInt(63), BigInt(63)), (9, BigInt(31), BigInt(-1)),
          (10, BigInt(31), BigInt(31)), (12, BigInt(0x7ff), BigInt(-1)),
          (13, BigInt(0x7fff), BigInt(-1)), (11, BigInt("80000000", 16), BigInt(-2147483648L)),
          (15, BigInt(63), BigInt(63)))
        for ((selector, minimum, expected) <- oldImmediates) {
          dut.io.rawSelector.poke(selector.U)
          dut.io.rawMinBits.poke(minimum.U)
          dut.clock.step()
          dut.io.rawImmediate.expect((expected & mask64).U)
          cases += 1
        }
        // Fixed standard RVV encodings exercise the real table as well as the extractor.
        for ((word, selector, minimum, expected) <- Seq(
          ("022fb0d7", 9, BigInt(31), BigInt(-1)),
          ("962fb0d7", 10, BigInt(31), BigInt(31)),
          ("562fb0d7", 15, BigInt(63), BigInt(63)),
          ("004170d7", 12, BigInt(4), BigInt(4)),
          ("c04ff0d7", 13, BigInt(0x7c04), BigInt(-1020)))) {
          dut.io.instruction.poke(BigInt(word, 16).U)
          dut.clock.step()
          dut.io.illegal.expect(false.B)
          dut.io.selector.expect(selector.U)
          dut.io.minBits.expect(minimum.U)
          dut.io.decodedImmediate.expect((expected & mask64).U)
          cases += 1
        }
        dut.io.ftqPC.poke(BigInt("80000000", 16).U)
        dut.io.oldRs1.poke(BigInt("80010001", 16).U)
        dut.io.slot.poke(3.U)
        // Independently fixed ordinary instructions: JAL x1,+8; JALR x1,x2,-1; AUIPC x1,1.
        for ((word, target, result) <- Seq(
          ("008000ef", "8000000e", "8000000a"),
          ("fff100e7", "80010000", "8000000a"),
          ("00001097", "80001006", "80001006"))) {
          dut.io.instruction.poke(BigInt(word, 16).U)
          dut.clock.step()
          dut.io.illegal.expect(false.B)
          dut.io.target.expect(BigInt(target, 16).U)
          dut.io.result.expect(BigInt(result, 16).U)
          cases += 1
        }
        dut.io.instruction.poke(BigInt("000100e7", 16).U)
        dut.io.isRVC.poke(true.B)
        dut.clock.step()
        dut.io.result.expect(BigInt("80000008", 16).U)
        println(s"FDICall decode arithmetic PASS enabled=$enabled cases=${cases + 1} backend=verilator")
      }
    }
  }
}
