// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.decode.Imm_Z
import xiangshan.backend.fu.wrapper.CSR

class UserTimerCSRHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new Bundle {
      val instruction = UInt(32.W)
      val operation = UInt(6.W)
      val source = UInt(64.W)
    }))
    val response = Decoupled(new Bundle {
      val data = UInt(64.W)
      val illegal = Bool()
      val virtualIllegal = Bool()
    })
    val mode = Output(UInt(2.W))
    val virtualMode = Output(Bool())
    val debugMode = Output(Bool())
  })

  val csr = Module(new CSR(FuConfig.CsrCfg))

  // Tie off unrelated environment inputs while preserving the production wrapper and NewCSR.
  def tieInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(tieInputs)
    case vector: Vec[_] => vector.foreach(tieInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input =>
      leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  tieInputs(csr.io)
  csr.huEntry.foreach { port =>
    tieInputs(port)
    port.completion.ready := true.B
  }

  csr.io.in.valid := io.request.valid
  csr.io.in.bits.ctrl.fuOpType := io.request.bits.operation
  csr.io.in.bits.data.imm := Imm_Z().minBitsFromInstr(io.request.bits.instruction)
  csr.io.in.bits.data.src(0) := io.request.bits.source
  io.request.ready := csr.io.in.ready
  csr.io.out.ready := io.response.ready
  io.response.valid := csr.io.out.valid
  io.response.bits.data := csr.io.out.bits.res.data
  io.response.bits.illegal := csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr)
  io.response.bits.virtualIllegal := csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.virtualInstr)
  io.mode := csr.io.csrio.get.tlb.priv.imode
  io.virtualMode := csr.io.csrio.get.customCtrl.virtMode
  io.debugMode := csr.io.csrio.get.debugMode
}

class UserTimerCSRTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "User timer production CSR integration"

  private val bankAddresses = Seq(0x000, 0x004, 0x005, 0x040, 0x041, 0x042, 0x043, 0x044, 0x800)
  private val allOnes = (BigInt(1) << 64) - 1

  private def parameters(enabled: Boolean): Parameters = {
    val base = new top.DefaultConfig
    base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasUserTimerInterrupt = enabled)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
  }

  for (enabled <- Seq(false, true)) {
    it should s"advertise only the selected experimental capability with enabled=$enabled" in {
      val runRoot = java.nio.file.Paths.get(sys.props("uit02.runRoot")).toRealPath()
      assert(java.nio.file.Paths.get("").toRealPath() == runRoot)
      assert(!XSCoreParameters().HasUserTimerInterrupt)
      implicit val p: Parameters = parameters(enabled)
      test(new UserTimerCSRHarness).withAnnotations(Seq(
        VerilatorBackendAnnotation,
        TargetDirAnnotation(s"capability-${if (enabled) "on" else "off"}")
      )) { dut =>
        dut.io.request.valid.poke(false.B)
        dut.io.request.bits.instruction.poke(0.U)
        dut.io.request.bits.operation.poke(0.U)
        dut.io.request.bits.source.poke(0.U)
        dut.io.response.ready.poke(true.B)
        dut.reset.poke(true.B)
        dut.clock.step(5)
        dut.reset.poke(false.B)
        dut.clock.step(2)
        dut.io.mode.expect(3.U)
        dut.io.virtualMode.expect(false.B)

        def access(addr: Int, source: BigInt = 0, write: Boolean = false, illegal: Boolean = false): BigInt = {
          dut.io.request.ready.expect(true.B)
          val function = if (write) 1 else 2
          val rs1 = if (write) 1 else 0
          val instruction = (BigInt(addr) << 20) | (BigInt(rs1) << 15) | (function << 12) | (1 << 7) | 0x73
          dut.io.request.bits.instruction.poke(instruction.U)
          dut.io.request.bits.operation.poke((if (write) CSROpType.wrt else CSROpType.set))
          dut.io.request.bits.source.poke(source.U)
          dut.io.request.valid.poke(true.B)
          dut.clock.step()
          dut.io.request.valid.poke(false.B)
          dut.io.response.valid.expect(true.B)
          dut.io.response.bits.illegal.expect(illegal.B)
          dut.io.response.bits.virtualIllegal.expect(false.B)
          val result = dut.io.response.bits.data.peek().litValue
          dut.clock.step()
          dut.io.response.valid.expect(false.B)
          result
        }

        val expectedMisa = BigInt("80000000003411af", 16) | (if (enabled) BigInt(1) << 23 else BigInt(0))
        assert(access(0x301) == expectedMisa)
        access(0x301, allOnes, write = true)
        assert(access(0x301) == expectedMisa)
        access(0x301, 0, write = true)
        assert(access(0x301) == expectedMisa)
        access(0x340, BigInt("fedcba9876543210", 16), write = true)
        assert(access(0x340) == BigInt("fedcba9876543210", 16))
        if (!enabled) bankAddresses.foreach { addr =>
          access(addr, illegal = true)
          access(addr, allOnes, write = true, illegal = true)
        }
        println(s"UserTimer CSR capability PASS: enabled=$enabled misa=0x${expectedMisa.toString(16)} " +
          s"wrapper=production disabledAddressChecks=${if (enabled) 0 else bankAddresses.size * 2}")
      }
    }
  }
}
