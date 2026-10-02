// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.decode.Imm_Z
import xiangshan.backend.fu.wrapper.CSR

object FDICSRTestAddresses {
  // Independent enumeration: do not derive expected coverage from production maps.
  val all = Seq(0xbc4, 0xbc5, 0xbc6, 0x9e1, 0x9e2, 0x9e3, 0x880) ++
    (0x890 to 0x8af) ++ (0x8b0 to 0x8b3) ++ (0x8c0 to 0x8c8)
}

// Observe existing production ports only; there are no test state overrides.
class FDICSRProbe(implicit p: Parameters) extends CSR(FuConfig.CsrCfg) {
  val observed = IO(Output(new Bundle {
    val state = Vec(52, UInt(64.W))
    val writes = UInt(52.W)
    val pc = UInt(64.W)
    val mode = UInt(2.W)
    val virtualMode = Bool()
    val aiaRequest = Bool()
    val aiaWrite = Bool()
  }))
  val addresses = FDICSRTestAddresses.all
  for ((address, index) <- addresses.zipWithIndex) {
    observed.state(index) := csrMod.csrOutMap.get(address).map(BoringUtils.bore(_)).getOrElse(0.U)
  }
  observed.writes := VecInit(addresses.map(address =>
    csrMod.csrRwMap.get(address).map(entry => BoringUtils.bore(entry._1.wen)).getOrElse(false.B))).asUInt
  observed.pc := csrMod.io.in.bits.sourcePc
  observed.mode := csrMod.io.status.privState.PRVM.asUInt
  observed.virtualMode := csrMod.io.status.privState.V.asUInt.asBool
  observed.aiaRequest := csrMod.toAIA.addr.valid
  observed.aiaWrite := csrMod.toAIA.wdata.valid
}

class FDICSRIntegrationHarness(implicit val p: Parameters) extends Module with HasXSParameter {
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new Bundle {
      val instruction = UInt(32.W)
      val operation = UInt(6.W)
      val operand = UInt(64.W)
      val basePc = UInt(50.W)
      val offset = UInt(log2Up(PredictWidth).W)
      val rob = UInt(log2Ceil(RobSize).W)
    }))
    val response = Decoupled(new Bundle {
      val data = UInt(64.W)
      val illegal = Bool()
      val virtualIllegal = Bool()
      val rob = UInt(log2Ceil(RobSize).W)
    })
    val flush = Input(Bool())
    val flushRob = Input(UInt(log2Ceil(RobSize).W))
    val trap = Input(Bool())
    val state = Output(Vec(52, UInt(64.W)))
    val writes = Output(UInt(52.W))
    val pc = Output(UInt(64.W))
    val mode = Output(UInt(2.W))
    val virtualMode = Output(Bool())
    val aiaRequest = Output(Bool())
    val aiaWrite = Output(Bool())
  })
  val csr = Module(new FDICSRProbe)
  def tieInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(tieInputs)
    case vector: Vec[_] => vector.foreach(tieInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  tieInputs(csr.io)
  csr.huEntry.foreach { port => tieInputs(port); port.completion.ready := true.B }
  csr.io.in.valid := io.request.valid
  csr.io.in.bits.ctrl.fuOpType := io.request.bits.operation
  csr.io.in.bits.ctrl.robIdx.value := io.request.bits.rob
  csr.io.in.bits.ctrl.ftqOffset.get := io.request.bits.offset
  csr.io.in.bits.data.imm := Imm_Z().minBitsFromInstr(io.request.bits.instruction)
  csr.io.in.bits.data.src(0) := io.request.bits.operand
  csr.io.in.bits.data.pc.get := io.request.bits.basePc
  io.request.ready := csr.io.in.ready
  csr.io.out.ready := io.response.ready
  io.response.valid := csr.io.out.valid
  io.response.bits.data := csr.io.out.bits.res.data
  io.response.bits.illegal := csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr)
  io.response.bits.virtualIllegal := csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.virtualInstr)
  io.response.bits.rob := csr.io.out.bits.ctrl.robIdx.value
  csr.io.flush.valid := io.flush
  csr.io.flush.bits.level := RedirectLevel.flush
  csr.io.flush.bits.robIdx.value := io.flushRob
  csr.io.csrio.get.exception.valid := io.trap
  csr.io.csrio.get.exception.bits.pc := "h80001000".U
  csr.io.csrio.get.exception.bits.instr := "h00000073".U
  csr.io.csrio.get.exception.bits.exceptionVec(ExceptionNO.ecallU) := io.trap
  io.state := csr.observed.state
  io.writes := csr.observed.writes
  io.pc := csr.observed.pc
  io.mode := csr.observed.mode
  io.virtualMode := csr.observed.virtualMode
  io.aiaRequest := csr.observed.aiaRequest
  io.aiaWrite := csr.observed.aiaWrite
}

class FDICSRIntegrationTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDI production CSR access"
  private val addresses = FDICSRTestAddresses.all
  private val allOnes = (BigInt(1) << 64) - 1
  private val masks = addresses.map { address =>
    val mask = address match {
      case 0xbc4 => BigInt(0x7ff)
      case 0x9e1 => BigInt(0x7c2)
      case 0x880 => BigInt("bbbbbbbbbbbbbbbb", 16)
      case 0x8c8 => BigInt("0001000100010001", 16)
      case 0x8b3 => BigInt(7)
      case 0x8b0 | 0x8b1 | 0x8b2 => allOnes
      case _ => allOnes ^ 7
    }
    address -> mask
  }.toMap

  private def run(name: String, enabled: Boolean = true)(body: Driver => Unit): Unit = {
    val root = java.nio.file.Paths.get(sys.props("c05.runRoot")).toRealPath()
    require(java.nio.file.Paths.get("").toRealPath() == root)
    val base = new top.DefaultConfig
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
    test(new FDICSRIntegrationHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation(name))) { dut =>
      val driver = new Driver(dut, enabled)
      driver.reset()
      body(driver)
      driver.checkAll()
      println(s"FDI CSR PASS group=$name cycles=${driver.cycles} transactions=${driver.transactions} " +
        s"writes=${driver.writes} backend=verilator wrapper=production")
    }
  }

  private class Driver(val dut: FDICSRIntegrationHarness, enabled: Boolean) {
    val expected = scala.collection.mutable.Map.from(addresses.map(_ -> BigInt(0)))
    var cycles = 0
    var transactions = 0
    var writes = 0
    var pc = BigInt(0x1000)
    def edge(): Unit = { dut.clock.step(); cycles += 1 }
    def reset(): Unit = {
      dut.io.request.valid.poke(false.B)
      drive(0x340)
      dut.io.response.ready.poke(true.B)
      dut.io.flush.poke(false.B)
      dut.io.flushRob.poke(4.U)
      dut.io.trap.poke(false.B)
      dut.reset.poke(true.B)
      for (_ <- 0 until 5) edge()
      dut.reset.poke(false.B)
      expected.keys.foreach(expected(_) = 0)
      pc = 0x1000
      edge()
      checkAll()
    }
    def drive(address: Int, function: Int = 2, rs1: Int = 0, rd: Int = 1,
      operand: BigInt = 0, rob: Int = 4, offset: Int = 0): Unit = {
      val instruction = (BigInt(address) << 20) | (BigInt(rs1) << 15) |
        (function << 12) | (rd << 7) | 0x73
      dut.io.request.bits.instruction.poke(instruction.U)
      dut.io.request.bits.operation.poke((8 | function).U)
      dut.io.request.bits.operand.poke(operand.U)
      dut.io.request.bits.rob.poke(rob.U)
      dut.io.request.bits.basePc.poke((pc & ((BigInt(1) << 50) - 1)).U)
      dut.io.request.bits.offset.poke(offset.U)
    }
    def checkAll(): Unit = addresses.zipWithIndex.foreach { case (address, index) =>
      withClue(f"cycle=$cycles address=0x$address%x ") {
        dut.io.state(index).expect((if (enabled) expected(address) else BigInt(0)).U)
      }
    }
    def access(address: Int, function: Int = 2, rs1: Int = 0, rd: Int = 1,
      operand: BigInt = 0, illegal: Boolean = false, stalls: Int = 0,
      cancelC0: Boolean = false, cancelC1: Boolean = false,
      presentYounger: Boolean = false, flushYounger: Boolean = false,
      cancelResponse: Boolean = false,
      idleAddress: Int = 0x8b2, noAia: Boolean = false,
      offset: Int = 0, checkPc: Option[BigInt] = None): BigInt = {
      dut.io.request.ready.expect(true.B)
      drive(address, function, rs1, rd, operand, offset = offset)
      dut.io.request.valid.poke(true.B)
      dut.io.response.ready.poke((stalls == 0).B)
      dut.io.flushRob.poke(4.U)
      dut.io.flush.poke(cancelC0.B)
      checkPc.foreach(value => dut.io.pc.expect(value.U))
      val old = expected.get(address)
      val shouldWrite = (function & 3) == 1 || rs1 != 0
      val bankWrite = enabled && old.nonEmpty && shouldWrite && !illegal && !cancelC0 && !cancelC1
      val op = if (function >= 5) BigInt(rs1) else operand
      val value = old.map { previous => (function & 3) match {
        case 1 => op
        case 2 => previous | op
        case 3 => previous & (allOnes ^ op)
      }}
      dut.io.writes.expect(0.U)
      edge()
      transactions += 1
      dut.io.request.valid.poke(presentYounger.B)
      // A different valid input during C1 must not replace the accepted address,
      // final data, read result, permission, or ROB cancellation identity.
      if (old.nonEmpty) drive(idleAddress, 1, 1, 1, allOnes, rob = 7)
      dut.io.flushRob.poke((if (flushYounger) 7 else 4).U)
      dut.io.flush.poke((cancelC1 || flushYounger).B)
      val responds = !cancelC0 && !cancelC1
      dut.io.response.valid.expect(responds.B)
      val result = dut.io.response.bits.data.peek().litValue
      if (responds) {
        dut.io.response.bits.illegal.expect(illegal.B)
        if (enabled) dut.io.response.bits.virtualIllegal.expect(false.B)
        dut.io.response.bits.rob.expect(4.U)
        if (!illegal) old.foreach(v => assert(result == v, f"old view 0x$address%x"))
      }
      val pulse = if (bankWrite) BigInt(1) << addresses.indexOf(address) else BigInt(0)
      dut.io.writes.expect(pulse.U)
      if (noAia) {
        dut.io.request.ready.expect(false.B)
        dut.io.aiaRequest.expect(false.B)
        dut.io.aiaWrite.expect(false.B)
      }
      edge()
      if (bankWrite) {
        val next = value.get & masks(address)
        if (address == 0x9e1) {
          expected(0xbc4) = (expected(0xbc4) & 0x3d) | next
          expected(0x9e1) = next
        } else if (address == 0xbc4) {
          expected(0xbc4) = next
          expected(0x9e1) = next & 0x7c2
        } else expected(address) = next
        writes += 1
      }
      dut.io.flush.poke(false.B)
      if (cancelResponse) {
        require(stalls > 0 && responds && bankWrite)
        dut.io.flushRob.poke(4.U)
        dut.io.flush.poke(true.B)
        dut.io.response.valid.expect(false.B)
        dut.io.writes.expect(0.U)
        if (noAia) {
          dut.io.aiaRequest.expect(false.B)
          dut.io.aiaWrite.expect(false.B)
        }
        edge()
        dut.io.flush.poke(false.B)
      }
      for (_ <- 0 until stalls if responds && !cancelResponse) {
        dut.io.response.valid.expect(true.B)
        dut.io.response.bits.data.expect(result.U)
        dut.io.response.bits.illegal.expect(illegal.B)
        dut.io.response.bits.rob.expect(4.U)
        dut.io.writes.expect(0.U)
        dut.io.request.ready.expect(false.B)
        if (noAia) {
          dut.io.aiaRequest.expect(false.B)
          dut.io.aiaWrite.expect(false.B)
        }
        edge()
      }
      dut.io.request.valid.poke(false.B)
      if (stalls > 0 && responds && !cancelResponse) {
        dut.io.response.ready.poke(true.B)
        edge()
      }
      dut.io.response.valid.expect(false.B)
      dut.io.writes.expect(0.U)
      if (old.nonEmpty) checkAll()
      result
    }
    def resetAcceptedWrite(): Unit = {
      dut.io.response.ready.poke(false.B)
      drive(0x8b1, 1, 1, operand = allOnes)
      dut.io.request.valid.poke(true.B)
      dut.io.request.ready.expect(true.B)
      edge()
      dut.io.request.valid.poke(false.B)
      dut.reset.poke(true.B)
      edge()
      dut.reset.poke(false.B)
      expected.keys.foreach(expected(_) = 0)
      dut.io.response.ready.poke(true.B)
      dut.io.response.valid.expect(false.B)
      dut.io.writes.expect(0.U)
      edge()
      checkAll()
    }
    def write(address: Int, value: BigInt): Unit = access(address, 1, 1, operand = value)
    def mode(privilege: Int, virtual: Boolean = false): Unit = {
      write(0x300, (BigInt(privilege) << 11) | (if (virtual) BigInt(1) << 39 else BigInt(0)))
      write(0x341, 0x1000)
      dut.io.request.bits.instruction.poke("h30200073".U)
      dut.io.request.bits.operation.poke(CSROpType.jmp)
      dut.io.request.valid.poke(true.B)
      edge()
      dut.io.request.valid.poke(false.B)
      dut.io.response.valid.expect(true.B)
      dut.io.response.bits.illegal.expect(false.B)
      edge()
      dut.io.mode.expect(privilege.U)
      dut.io.virtualMode.expect(virtual.B)
    }
    def stateen(machine: Boolean, supervisor: Boolean): Unit = {
      write(0x30c, (BigInt(1) << 63) | (if (machine) BigInt(1) else BigInt(0)))
      write(0x60c, (BigInt(1) << 63) | 1)
      write(0x10c, if (supervisor) 1 else 0)
    }
  }

  private def checkStorage(d: Driver): Unit = {
    addresses.foreach { address =>
      d.access(address)
      for (f <- Seq(1, 2, 3, 5, 6, 7)) {
        d.access(address, f, 31, operand = allOnes, stalls = 2, presentYounger = true)
        d.access(address, f, 0, rd = 0, operand = 0)
        d.access(address, f, 1, operand = 0)
      }
    }
    d.write(0xbc4, 0x7ff)
    d.write(0x9e1, 0)
    assert(d.expected(0xbc4) == 0x3d)
    for (address <- addresses; cancel <- Seq(0, 1)) {
      d.access(address, 1, 1, operand = allOnes,
        cancelC0 = cancel == 0, cancelC1 = cancel == 1)
    }
    d.access(0x8b1, 1, 1, operand = 0x1235, stalls = 3,
      presentYounger = true, flushYounger = true)
    d.access(0x8b1, 1, 1, operand = 0x5679, stalls = 3, cancelResponse = true)
    assert(d.expected(0x8b1) == 0x5679)
    d.resetAcceptedWrite()
    for (address <- Seq(0xbc3, 0x9e0, 0x881, 0x8b4, 0x8c9))
      d.access(address, 1, 1, operand = allOnes, illegal = true)
    d.write(0x340, 0x1357)
    assert(d.access(0x340) == 0x1357)
    d.reset()
  }

  private def checkPermissions(d: Driver): Unit = {
    var rows = 0
    for ((privilege, virtual) <- Seq((3, false), (1, false), (0, false), (1, true), (0, true));
      machine <- Seq(false, true); supervisor <- Seq(false, true);
      enabled <- Seq(false, true); trusted <- Seq(false, true)) {
      d.reset()
      d.stateen(machine, supervisor)
      d.write(0xbc5, 0x1000); d.write(0xbc6, 0x1100)
      d.write(0x9e2, 0x1000); d.write(0x9e3, 0x1100)
      d.write(0xbc4, if (enabled) 3 else 0)
      d.mode(privilege, virtual)
      d.pc = if (trusted) 0x1000 else 0x1100
      for (address <- addresses) {
        val encoded = (address >> 8) & 3
        val allowed = !virtual && privilege >= encoded && (privilege == 3 ||
          machine && (privilege == 1 || supervisor) && (!enabled || trusted))
        d.access(address, 2, 1, operand = 0, illegal = !allowed)
      }
      rows += 1
    }
    assert(rows == 80)
    println(s"FDI permission rows=$rows addresses=${addresses.size}")
    // An untrusted HS source cannot modify the U view even when clearing it.
    d.reset(); d.stateen(true, true)
    d.write(0xbc5, 0x1000); d.write(0xbc6, 0x1100); d.write(0xbc4, 3)
    d.mode(1); d.pc = 0x1100
    d.access(0x9e1, 1, 1, operand = 0, illegal = true)
  }

  private def checkSourcePc(d: Driver): Unit = {
    for ((mode, negative) <- Seq((8, BigInt("ffffffc000001000", 16)),
      (9, BigInt("ffff800000001000", 16)))) {
      d.reset(); d.stateen(true, true)
      d.write(0xbc5, negative); d.write(0xbc6, negative + 0x100)
      d.write(0xbc4, 1)
      d.write(0x180, BigInt(mode) << 60)
      d.mode(1)
      d.pc = negative
      d.access(0x8b0, offset = 3, checkPc = Some(negative + 6))
      d.pc = negative + 0xfe
      d.access(0x8b0, offset = 1, checkPc = Some(negative + 0x100), illegal = true)
    }
    d.reset(); d.pc = BigInt("800000001000", 16)
    d.access(0x8b0, offset = 3, checkPc = Some(d.pc + 6))
  }

  it should "isolate FDI reads from subsequent live AIA addresses" in run("c05-aia-isolation") { d =>
    // Configure the actual production select CSRs into their IMSIC ranges.
    // HS without custom-state permission then exercises the rejected-read case.
    for (denied <- Seq(false, true); target <- Seq(0x351, 0x151);
      youngerValid <- Seq(false, true)) {
      d.reset()
      d.write(0x350, 0x70)
      d.write(0x150, 0x70)
      if (denied) { d.stateen(false, false); d.mode(1) }
      d.access(0x8b0, illegal = denied, stalls = 3,
        presentYounger = youngerValid, idleAddress = target, noAia = true)
    }
  }

  it should "preserve state transactions permissions and source PCs through the production wrapper" in run("c05-on") { d =>
    checkStorage(d)
    checkPermissions(d)
    checkSourcePc(d)
  }

  it should "leave FDI absent in the production closed configuration" in run("c05-off", false) { d =>
    for ((privilege, virtual) <- Seq((3, false), (1, false), (0, false), (1, true), (0, true))) {
      d.reset(); d.stateen(true, true); d.mode(privilege, virtual)
      addresses.foreach(address => d.access(address, 1, 1, operand = allOnes, illegal = true))
    }
  }
}
