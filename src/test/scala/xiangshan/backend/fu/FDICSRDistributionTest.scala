// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import utility.DelayN
import xiangshan._
import xiangshan.backend.fu.NewCSR.{FDICSRMirror, FDIMirrorClient}

class FDICSRDistributionHarness(implicit p: Parameters) extends FDICSRIntegrationHarness {
  val observed = IO(Output(new Bundle {
    val bus = new DistributedCSRIO
    val frontend = Vec(5, UInt(64.W))
    val control = Vec(13, UInt(64.W))
    val memory = Vec(34, UInt(64.W))
    val frontendInput = new DistributedCSRIO
    val memoryInput = new DistributedCSRIO
  }))
  val bus = csr.io.csrio.get.customCtrl.distribute_csr
  observed.bus := bus
  observed.frontend := 0.U.asTypeOf(observed.frontend)
  observed.control := 0.U.asTypeOf(observed.control)
  observed.memory := 0.U.asTypeOf(observed.memory)
  observed.frontendInput := 0.U.asTypeOf(observed.frontendInput)
  observed.memoryInput := 0.U.asTypeOf(observed.memoryInput)
  if (HasFDI) {
    val control = Module(new FDICSRMirror(FDIMirrorClient.ControlFlow))
    val frontend = Module(new FDICSRMirror(FDIMirrorClient.Frontend))
    val memory = Module(new FDICSRMirror(FDIMirrorClient.Memory))
    control.io.distribute := bus
    frontend.io.distribute := DelayN(bus, 2)
    memory.io.distribute := DelayN(bus, 2)
    frontend.io.distribute.w.valid := RegNext(RegNext(bus.w.valid, false.B), false.B)
    memory.io.distribute.w.valid := RegNext(RegNext(bus.w.valid, false.B), false.B)
    observed.control := control.io.state
    observed.frontend := frontend.io.state
    observed.memory := memory.io.state
    observed.frontendInput := frontend.io.distribute
    observed.memoryInput := memory.io.distribute
  }
}

class FDICSRDistributionTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDI final-value distribution"
  private val ones = (BigInt(1) << 64) - 1
  private val all = Seq(0xbc4, 0xbc5, 0xbc6, 0x9e1, 0x9e2, 0x9e3, 0x880) ++
    (0x890 to 0x8af) ++ (0x8b0 to 0x8b3) ++ (0x8c0 to 0x8c8)
  private val frontendAddresses = Seq(0xbc4, 0xbc5, 0xbc6, 0x9e2, 0x9e3)
  private val controlAddresses = Seq(0xbc4, 0x8c8) ++ (0x8c0 to 0x8c7) ++ Seq(0x8b0, 0x8b1, 0x8b2)
  private val memoryAddresses = Seq(0xbc4, 0x880) ++ (0x890 to 0x8af)
  private val masks = all.map { address => address -> (address match {
    case 0xbc4 => BigInt(0x7ff)
    case 0x9e1 => BigInt(0x7c2)
    case 0x880 => BigInt("bbbbbbbbbbbbbbbb", 16)
    case 0x8c8 => BigInt("0001000100010001", 16)
    case 0x8b3 => BigInt(7)
    case 0x8b0 | 0x8b1 | 0x8b2 => ones
    case _ => ones ^ 7
  }) }.toMap
  private def backing(address: Int): Int = if (address == 0x9e1) 0xbc4 else address

  private def run(name: String, enabled: Boolean)(body: Driver => Unit): Unit = {
    val root = java.nio.file.Paths.get(sys.props("c06.runRoot")).toRealPath()
    require(java.nio.file.Paths.get("").toRealPath() == root)
    val base = new top.DefaultConfig
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
    test(new FDICSRDistributionHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation(name))) { dut =>
      val driver = new Driver(dut, enabled)
      driver.initialize()
      body(driver)
      driver.idle(3)
      println(s"FDI distribution PASS group=$name cycles=${driver.cycles} " +
        s"requests=${driver.requests} fdiEffects=${driver.effects} " +
        s"frontendWords=5 controlWords=13 memoryWords=34")
    }
  }

  private class Driver(val dut: FDICSRDistributionHarness, enabled: Boolean) {
    private val bank = scala.collection.mutable.Map.from(all.map(backing).distinct.map(_ -> BigInt(0)))
    private val front = scala.collection.mutable.Map.from(frontendAddresses.map(_ -> BigInt(0)))
    private val control = scala.collection.mutable.Map.from(controlAddresses.map(_ -> BigInt(0)))
    private val memory = scala.collection.mutable.Map.from(memoryAddresses.map(_ -> BigInt(0)))
    // A queue records accepted write effects, independently of the RTL delay registers.
    private var transport = Vector.fill[Option[(Int, BigInt)]](2)(None)
    var cycles = 0
    var requests = 0
    var effects = 0

    private def updateMirror(state: scala.collection.mutable.Map[Int, BigInt], token: Option[(Int, BigInt)]): Unit = {
      token.foreach { case (address, data) =>
        val key = backing(address)
        if (state.contains(key)) state(key) = data
      }
    }
    private def check(): Unit = {
      for ((address, index) <- all.zipWithIndex) {
        val value = if (enabled) bank(backing(address)) & masks(address) else BigInt(0)
        withClue(f"cycle=$cycles owner=0x$address%x ") { dut.io.state(index).expect(value.U) }
      }
      for ((addresses, state, actual) <- Seq(
        (frontendAddresses, front, dut.observed.frontend),
        (controlAddresses, control, dut.observed.control),
        (memoryAddresses, memory, dut.observed.memory)); (address, index) <- addresses.zipWithIndex) {
        withClue(f"cycle=$cycles mirror=0x$address%x ") {
          actual(index).expect((if (enabled) state(address) else BigInt(0)).U)
        }
      }
    }
    def edge(token: Option[(Int, BigInt)] = None, resetting: Boolean = false): Unit = {
      dut.reset.poke(resetting.B)
      val actual = dut.observed.bus.w
      actual.valid.expect(token.nonEmpty.B)
      token.foreach { case (address, data) =>
        actual.bits.addr.expect(address.U)
        actual.bits.data.expect(data.U)
      }
      val ownerWrite = token.filter(t => enabled && masks.contains(t._1))
      dut.io.writes.expect(ownerWrite.map(t => BigInt(1) << all.indexOf(t._1)).getOrElse(BigInt(0)).U)
      if (!resetting) {
        for (input <- Seq(dut.observed.frontendInput.w, dut.observed.memoryInput.w)) {
          val delayed = if (enabled) transport.head else None
          input.valid.expect(delayed.nonEmpty.B)
          delayed.foreach { case (address, data) =>
            input.bits.addr.expect(address.U)
            input.bits.data.expect(data.U)
          }
        }
      }
      if (resetting) {
        Seq(bank, front, control, memory).foreach(state => state.keys.foreach(key => state(key) = 0))
        transport = Vector.fill(2)(None)
      } else {
        if (enabled) {
          token.filter(t => masks.contains(t._1)).foreach { case (address, data) =>
            bank(backing(address)) = data
            effects += 1
          }
          updateMirror(control, token)
          updateMirror(front, transport.head)
          updateMirror(memory, transport.head)
        }
        transport = transport.tail :+ token
      }
      dut.clock.step()
      cycles += 1
      check()
    }
    def drive(address: Int, function: Int = 2, rs1: Int = 0, rd: Int = 1, operand: BigInt = 0): Unit = {
      dut.io.request.bits.instruction.poke(((BigInt(address) << 20) | (BigInt(rs1) << 15) |
        (function << 12) | (rd << 7) | 0x73).U)
      dut.io.request.bits.operation.poke((8 | function).U)
      dut.io.request.bits.operand.poke(operand.U)
      dut.io.request.bits.basePc.poke(0x1000.U)
      dut.io.request.bits.offset.poke(0.U)
      dut.io.request.bits.rob.poke(4.U)
    }
    def initialize(): Unit = {
      dut.io.request.valid.poke(false.B)
      drive(0x340)
      dut.io.response.ready.poke(true.B)
      dut.io.flush.poke(false.B)
      dut.io.flushRob.poke(4.U)
      dut.io.trap.poke(false.B)
      // Settle reset before checking combinational observations of resetless legacy registers.
      dut.reset.poke(true.B); dut.clock.step(5); cycles += 5
      edge(resetting = true)
      dut.reset.poke(false.B)
      edge()
    }
    def idle(count: Int): Unit = {
      dut.io.request.valid.poke(false.B)
      for (_ <- 0 until count) edge()
    }
    def access(address: Int, function: Int = 2, rs1: Int = 0, rd: Int = 1,
      operand: BigInt = 0, illegal: Boolean = false, stalls: Int = 0,
      cancelBefore: Boolean = false, cancelEffect: Boolean = false,
      cancelAfter: Boolean = false, resetAfter: Int = -1): Unit = {
      require(resetAfter >= -1 && resetAfter <= 2)
      dut.io.request.ready.expect(true.B)
      drive(address, function, rs1, rd, operand)
      dut.io.request.valid.poke(true.B)
      dut.io.response.ready.poke((stalls == 0 && !cancelAfter).B)
      dut.io.flush.poke(cancelBefore.B)
      val fdi = masks.contains(address)
      val write = (function & 3) == 1 || rs1 != 0
      val old = if (fdi && enabled) bank(backing(address)) & masks(address) else BigInt(0)
      val source = if (function >= 5) BigInt(rs1) else operand
      val unmasked = (function & 3) match {
        case 1 => source
        case 2 => old | source
        case 3 => old & (ones ^ source)
      }
      val finalValue = if (address == 0x9e1) {
        (bank(0xbc4) & 0x3d) | (unmasked & 0x7c2)
      } else if (fdi) unmasked & masks(address) else unmasked
      val token = Option.when(write && !illegal && !cancelBefore && !cancelEffect)(address -> finalValue)
      edge()
      requests += 1
      dut.io.request.valid.poke(false.B)
      dut.io.flush.poke(cancelEffect.B)
      // Only FDI requests are deliberately followed by a changing live address;
      // the unrelated ordinary-CSR live-address behavior is not changed here.
      if (fdi) drive(0x8b2, 1, 1, operand = ones)
      dut.io.response.valid.expect((!cancelBefore && !cancelEffect).B)
      if (!cancelBefore && !cancelEffect) {
        dut.io.response.bits.illegal.expect(illegal.B)
        if (fdi && enabled && !illegal) dut.io.response.bits.data.expect(old.U)
      }
      if (resetAfter == 0) {
        edge(resetting = true)
        dut.reset.poke(false.B)
        dut.io.flush.poke(false.B)
        dut.io.response.ready.poke(true.B)
        idle(3)
      } else {
        edge(token)
        dut.io.flush.poke(false.B)
        if (resetAfter > 0) {
          for (_ <- 1 until resetAfter) edge()
          edge(resetting = true)
          dut.reset.poke(false.B)
          dut.io.response.ready.poke(true.B)
          idle(3)
        } else if (cancelAfter) {
          dut.io.flush.poke(true.B)
          dut.io.response.valid.expect(false.B)
          edge()
          dut.io.flush.poke(false.B)
          dut.io.response.ready.poke(true.B)
          idle(2)
        } else if (stalls > 0 && !cancelBefore && !cancelEffect) {
          for (_ <- 0 until stalls) {
            dut.io.response.valid.expect(true.B)
            dut.io.response.bits.illegal.expect(illegal.B)
            if (fdi && enabled && !illegal) dut.io.response.bits.data.expect(old.U)
            edge()
          }
          dut.io.response.ready.poke(true.B)
          edge()
        }
      }
    }
    def write(address: Int, value: BigInt): Unit = access(address, 1, 1, operand = value)
    def supervisor(): Unit = {
      write(0x300, BigInt(1) << 11)
      write(0x341, 0x1000)
      dut.io.request.bits.instruction.poke("h30200073".U)
      dut.io.request.bits.operation.poke(CSROpType.jmp)
      dut.io.request.valid.poke(true.B)
      edge()
      dut.io.request.valid.poke(false.B)
      dut.io.response.valid.expect(true.B)
      edge()
      dut.io.mode.expect(1.U)
    }
  }

  it should "publish one normalized word and maintain the exact local mirror latencies" in run("c06-on", true) { d =>
    for (address <- all; function <- Seq(1, 2, 3, 5, 6, 7)) {
      d.access(address, function, 31, operand = ones, stalls = 2)
      d.access(address, function, 0, rd = 0, operand = 0)
      d.access(address, function, 1, operand = 0)
    }
    // These writes are consecutive at the CSR slot's full accepted throughput.
    d.write(0xbc4, 0x7ff)
    d.write(0x9e1, 0)
    d.write(0x9e1, 0x7c2)
    d.write(0x8b1, BigInt("fedcba9876543211", 16))
    d.write(0x8b3, 7)
    for (address <- Seq(0xbc4, 0x9e1, 0x890, 0x8b1, 0x8c8)) {
      d.access(address, 1, 1, operand = ones, cancelBefore = true)
      d.access(address, 1, 1, operand = ones, cancelEffect = true)
      d.access(address, 1, 1, operand = ones, cancelAfter = true)
      for (resetAt <- 0 to 2) d.access(address, 1, 1, operand = ones, resetAfter = resetAt)
    }
    d.write(0x340, 0x1234)
    d.access(0x8b4, 1, 1, operand = ones, illegal = true)
    d.write(0x30c, BigInt(1) << 63)
    d.supervisor()
    d.access(0x8b0, 1, 1, operand = ones, illegal = true, stalls = 3)
  }

  it should "emit no FDI effect or mirror state when compiled out" in run("c06-off", false) { d =>
    all.foreach(address => d.access(address, 1, 1, operand = ones, illegal = true))
    d.write(0x340, 0x9876)
  }
}
