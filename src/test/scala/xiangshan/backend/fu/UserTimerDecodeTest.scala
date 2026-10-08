// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.decode.DecodeUnit

class UserTimerDecodeHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val instruction = Input(UInt(32.W))
    val fsOff = Input(Bool())
    val vsOff = Input(Bool())
    val illegal = Output(Bool())
    val virtualIllegal = Output(Bool())
    val isCSR = Output(Bool())
    val isJump = Output(Bool())
    val noSpecExec = Output(Bool())
    val blockBackward = Output(Bool())
  })

  val decoder = Module(new DecodeUnit)
  decoder.io.enq.ctrlFlow := 0.U.asTypeOf(decoder.io.enq.ctrlFlow)
  decoder.io.enq.vtype := 0.U.asTypeOf(decoder.io.enq.vtype)
  decoder.io.enq.vstart := 0.U
  decoder.io.csrCtrl := 0.U.asTypeOf(decoder.io.csrCtrl)
  decoder.io.fromCSR := 0.U.asTypeOf(decoder.io.fromCSR)
  decoder.io.enq.ctrlFlow.instr := io.instruction
  decoder.io.fromCSR.illegalInst.fsIsOff := io.fsOff
  decoder.io.fromCSR.illegalInst.vsIsOff := io.vsOff
  io.illegal := decoder.io.deq.decodedInst.exceptionVec(ExceptionNO.illegalInstr)
  io.virtualIllegal := decoder.io.deq.decodedInst.exceptionVec(ExceptionNO.virtualInstr)
  io.isCSR := FuType.FuTypeOrR(decoder.io.deq.decodedInst.fuType, FuType.csr)
  io.isJump := decoder.io.deq.decodedInst.fuOpType === CSROpType.jmp
  io.noSpecExec := decoder.io.deq.decodedInst.waitForward
  io.blockBackward := decoder.io.deq.decodedInst.blockBackward
}

class UserTimerDecodeTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "User timer decode boundary"

  for (enabled <- Seq(false, true)) {
    it should s"gate URET and preserve CSR FP and vector decode with enabled=$enabled" in {
      val runRoot = java.nio.file.Paths.get(sys.props("uit02.runRoot")).toRealPath()
      assert(java.nio.file.Paths.get("").toRealPath() == runRoot)
      val base = new top.DefaultConfig
      implicit val p: Parameters = base.alterPartial {
        case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
        case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
          EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
          EnableChiselDB = false, AlwaysBasicDB = false)
      }
      test(new UserTimerDecodeHarness).withAnnotations(Seq(
        VerilatorBackendAnnotation,
        TargetDirAnnotation(s"decode-${if (enabled) "on" else "off"}")
      )) { dut =>
        var cases = 0
        def check(instruction: BigInt, illegal: Boolean, csr: Boolean,
                  fsOff: Boolean = false, vsOff: Boolean = false): Unit = {
          dut.io.instruction.poke(instruction.U)
          dut.io.fsOff.poke(fsOff.B)
          dut.io.vsOff.poke(vsOff.B)
          dut.clock.step()
          dut.io.illegal.expect(illegal.B)
          dut.io.virtualIllegal.expect(false.B)
          if (!illegal) dut.io.isCSR.expect(csr.B)
          cases += 1
        }

        // Decode depends only on the extension flag; execution checks handler privilege.
        for (fsOff <- Seq(false, true); vsOff <- Seq(false, true)) {
          check(BigInt("00200073", 16), illegal = !enabled, csr = true, fsOff = fsOff, vsOff = vsOff)
          if (enabled) {
            dut.io.isJump.expect(true.B)
            dut.io.noSpecExec.expect(true.B)
            dut.io.blockBackward.expect(true.B)
          }
        }
        // Exact matching must leave adjacent unsupported SYSTEM encodings illegal.
        for (instruction <- Seq("00300073", "002000f3", "00208073")) {
          check(BigInt(instruction, 16), illegal = true, csr = false)
        }
        // Exact calls decode with the feature enabled; Jump checks source permission.
        // Vary operand and immediate bits without changing the frozen opcode/funct3 mask.
        val callMask = BigInt("0000707f", 16)
        for (pattern <- Seq(BigInt("0000000b", 16), BigInt("0000100b", 16));
             operands <- Seq(BigInt(0), BigInt("123a8f80", 16), BigInt("ffff8f80", 16))) {
          val instruction = pattern | (operands & (BigInt("ffffffff", 16) ^ callMask))
          assert((instruction & callMask) == pattern)
          check(instruction, illegal = !enabled, csr = false)
          if (enabled) {
            dut.io.noSpecExec.expect(true.B)
            dut.io.blockBackward.expect(true.B)
          }
        }
        for (function <- Seq(1, 2, 3, 5, 6, 7)) {
          val instruction = (BigInt(0x800) << 20) | (BigInt(1) << 15) | (function << 12) | (1 << 7) | 0x73
          check(instruction, illegal = false, csr = true)
        }
        for (instruction <- Seq("30200073", "10200073", "7b200073")) {
          check(BigInt(instruction, 16), illegal = false, csr = true)
        }
        // Representative arithmetic preserves the existing context-off exception behavior.
        check(BigInt("003100d3", 16), illegal = false, csr = false)
        check(BigInt("003100d3", 16), illegal = true, csr = false, fsOff = true)
        check(BigInt("022180d7", 16), illegal = false, csr = false)
        check(BigInt("022180d7", 16), illegal = true, csr = false, vsOff = true)
        println(s"UserTimer decode PASS: enabled=$enabled cases=$cases backend=verilator")
      }
    }
  }
}
