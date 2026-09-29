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

// Observation ports expose existing production IO without adding architectural state or overrides.
class UserTimerCSRProbe(implicit p: Parameters) extends CSR(FuConfig.CsrCfg) {
  val observed = IO(Output(new Bundle {
    val rawRead = UInt(64.W)
    val mode = UInt(2.W)
    val virtualMode = Bool()
    val debugMode = Bool()
    val aiaRequest = Bool()
    val aiaWrite = Bool()
  }))
  observed.rawRead := csrMod.io.out.bits.regOut
  observed.mode := csrMod.io.status.privState.PRVM.asUInt
  observed.virtualMode := csrMod.io.status.privState.V.asUInt.asBool
  observed.debugMode := csrMod.io.status.debugMode
  observed.aiaRequest := csrMod.toAIA.addr.valid
  observed.aiaWrite := csrMod.toAIA.wdata.valid
}

class UserTimerCSRIntegrationHarness(implicit p: Parameters) extends Module {
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
    val flush = Input(Bool())
    val trap = Input(Bool())
    val debugTrap = Input(Bool())
    val rawRead = Output(UInt(64.W))
    val mode = Output(UInt(2.W))
    val virtualMode = Output(Bool())
    val debugMode = Output(Bool())
    val aiaRequest = Output(Bool())
    val aiaWrite = Output(Bool())
  })

  val csr = Module(new UserTimerCSRProbe)
  def tieInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(tieInputs)
    case vector: Vec[_] => vector.foreach(tieInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
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
  csr.io.flush.valid := io.flush
  csr.io.flush.bits.level := RedirectLevel.flush
  csr.io.csrio.get.exception.valid := io.trap || io.debugTrap
  csr.io.csrio.get.exception.bits.pc := "h80001000".U
  csr.io.csrio.get.exception.bits.instr := "h00000073".U
  csr.io.csrio.get.exception.bits.exceptionVec(ExceptionNO.ecallU) := io.trap
  csr.io.csrio.get.exception.bits.singleStep := io.debugTrap
  io.rawRead := csr.observed.rawRead
  io.mode := csr.observed.mode
  io.virtualMode := csr.observed.virtualMode
  io.debugMode := csr.observed.debugMode
  io.aiaRequest := csr.observed.aiaRequest
  io.aiaWrite := csr.observed.aiaWrite
}

class UserTimerCSRIntegrationTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "User timer real CSR transactions"

  private val allOnes = (BigInt(1) << 64) - 1
  private val addresses = Seq(0x000, 0x004, 0x005, 0x040, 0x041, 0x042, 0x043, 0x044, 0x800)
  private val masks = Map(0x000 -> BigInt(0x11), 0x004 -> BigInt(0x10),
    0x005 -> (allOnes ^ 3), 0x040 -> allOnes, 0x041 -> (allOnes ^ 1),
    0x042 -> allOnes, 0x043 -> allOnes)
  private val modes = Seq(("M", 3, false), ("HS", 1, false), ("HU", 0, false),
    ("VS", 1, true), ("VU", 0, true))

  private def parameters(enabled: Boolean): Parameters = {
    val base = new top.DefaultConfig
    base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasUserTimerInterrupt = enabled)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
  }

  private def run(name: String, enabled: Boolean = true)(body: Driver => Unit): Unit = {
    val runRoot = java.nio.file.Paths.get(sys.props("uit02.runRoot")).toRealPath()
    assert(java.nio.file.Paths.get("").toRealPath() == runRoot)
    implicit val p: Parameters = parameters(enabled)
    test(new UserTimerCSRIntegrationHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation(name)
    )) { dut =>
      val driver = new Driver(dut, enabled)
      driver.reset()
      body(driver)
      println(s"UserTimer CSR integration PASS: group=$name cycles=${driver.cycles} " +
        s"transactions=${driver.transactions} backend=verilator wrapper=production")
    }
  }

  private class Driver(val dut: UserTimerCSRIntegrationHarness, enabled: Boolean) {
    var cycles = 0
    var transactions = 0
    // The oracle measures eligible time against an absolute deadline, independently of RTL subtraction.
    private var ticks = BigInt(0)
    private var deadline: Option[BigInt] = None
    def remaining: BigInt = deadline.map(d => (d - ticks).max(BigInt(0))).getOrElse(BigInt(0))
    def pending: Boolean = deadline.exists(_ <= ticks)

    def instruction(addr: Int, function: Int = 2, rs1: Int = 0, rd: Int = 1): BigInt =
      (BigInt(addr) << 20) | (BigInt(rs1) << 15) | (function << 12) | (rd << 7) | 0x73

    private def driveInstruction(addr: Int, function: Int = 2, rs1: Int = 0, rd: Int = 1, source: BigInt = 0): Unit = {
      dut.io.request.bits.instruction.poke(instruction(addr, function, rs1, rd).U)
      dut.io.request.bits.operation.poke((8 | function).U)
      dut.io.request.bits.source.poke(source.U)
    }

    def reset(): Unit = {
      dut.io.request.valid.poke(false.B)
      driveInstruction(0x800)
      dut.io.response.ready.poke(true.B)
      dut.io.flush.poke(false.B)
      dut.io.trap.poke(false.B)
      dut.io.debugTrap.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(5)
      cycles += 5
      dut.reset.poke(false.B)
      ticks = 0
      deadline = None
      edge()
      dut.io.mode.expect(3.U)
      dut.io.virtualMode.expect(false.B)
      dut.io.debugMode.expect(false.B)
      checkTimer()
    }

    def edge(write: Option[BigInt] = None): Unit = {
      val tick = dut.io.mode.peek().litValue == 0 && !dut.io.virtualMode.peek().litToBoolean &&
        !dut.io.debugMode.peek().litToBoolean
      dut.clock.step()
      cycles += 1
      if (enabled) {
        write match {
          case Some(value) => deadline = if (value == 0) None else Some(ticks + value)
          case None => if (tick) ticks += 1
        }
      }
    }

    def observe(addr: Int): BigInt = {
      dut.io.request.valid.expect(false.B)
      driveInstruction(addr)
      dut.io.rawRead.peek().litValue
    }

    def checkTimer(): Unit = if (enabled) {
      assert(observe(0x800) == remaining, s"remaining mismatch at cycle $cycles, expected $remaining")
      assert(observe(0x044) == (if (pending) BigInt(0x10) else BigInt(0)), s"pending mismatch at cycle $cycles")
    }

    def idle(count: Int): Unit = for (_ <- 0 until count) {
      dut.io.request.valid.poke(false.B)
      edge()
      checkTimer()
    }

    def access(addr: Int, function: Int = 2, rs1: Int = 0, source: BigInt = 0,
      rd: Int = 1, illegal: Boolean = false, stalls: Int = 0,
      idleAddress: Option[Int] = None, flushRequest: Boolean = false, flushWrite: Boolean = false,
      noAia: Boolean = false): BigInt = {
      dut.io.request.ready.expect(true.B)
      driveInstruction(addr, function, rs1, rd, source)
      dut.io.request.valid.poke(true.B)
      dut.io.response.ready.poke((stalls == 0).B)
      dut.io.flush.poke(flushRequest.B)
      val oldTimer = remaining
      val oldPending = pending
      val operand = if (function >= 5) BigInt(rs1) else source
      val writes = (function & 3) == 1 || ((function & 3) >= 2 && rs1 != 0)
      val writeValue = (function & 3) match {
        case 1 => operand
        case 2 => oldTimer | operand
        case 3 => oldTimer & (allOnes ^ operand)
      }
      val timerWrite = Option.when(enabled && addr == 0x800 && writes && !illegal &&
        !flushRequest && !flushWrite)(writeValue)
      edge()
      transactions += 1
      dut.io.request.valid.poke(false.B)
      dut.io.flush.poke(flushWrite.B)
      // Legacy CSR accesses retain their established write-phase address; new bank transactions must not depend on it.
      idleAddress.foreach(a => driveInstruction(a, source = allOnes ^ source))
      dut.io.response.valid.expect((!flushRequest && !flushWrite).B)
      val result = dut.io.response.bits.data.peek().litValue
      if (!flushRequest && !flushWrite) {
        dut.io.response.bits.illegal.expect(illegal.B)
        dut.io.response.bits.virtualIllegal.expect(false.B)
        if (enabled && !illegal && rd != 0 && addr == 0x800) assert(result == oldTimer)
        if (enabled && !illegal && rd != 0 && addr == 0x044) assert(result == (if (oldPending) BigInt(0x10) else BigInt(0)))
      }
      if (noAia) {
        dut.io.aiaRequest.expect(false.B)
        dut.io.aiaWrite.expect(false.B)
      }
      edge(timerWrite)
      dut.io.flush.poke(false.B)
      for (_ <- 0 until stalls) {
        dut.io.response.valid.expect(true.B)
        dut.io.response.bits.data.expect(result.U)
        dut.io.response.bits.illegal.expect(illegal.B)
        edge()
      }
      if (stalls > 0) {
        dut.io.response.ready.poke(true.B)
        edge()
      }
      dut.io.response.valid.expect(false.B)
      checkTimer()
      result
    }

    def write(addr: Int, value: BigInt, illegal: Boolean = false): Unit =
      access(addr, function = 1, rs1 = 1, source = value, illegal = illegal)

    def read(addr: Int, illegal: Boolean = false): BigInt = access(addr, illegal = illegal)

    def system(instruction: BigInt): Unit = {
      dut.io.request.ready.expect(true.B)
      dut.io.request.bits.instruction.poke(instruction.U)
      dut.io.request.bits.operation.poke(CSROpType.jmp)
      dut.io.request.valid.poke(true.B)
      edge()
      transactions += 1
      dut.io.request.valid.poke(false.B)
      dut.io.response.valid.expect(true.B)
      dut.io.response.bits.illegal.expect(false.B)
      dut.io.response.bits.virtualIllegal.expect(false.B)
      edge()
      checkTimer()
    }

    def mode(name: String, privilege: Int, virtual: Boolean): Unit = {
      dut.io.mode.expect(3.U)
      dut.io.virtualMode.expect(false.B)
      write(0x300, (BigInt(privilege) << 11) | (if (virtual) BigInt(1) << 39 else BigInt(0)))
      write(0x341, BigInt("80001000", 16))
      system(BigInt("30200073", 16))
      withClue(s"mode transition to $name: ") {
        dut.io.mode.expect(privilege.U)
        dut.io.virtualMode.expect(virtual.B)
      }
    }

    def machine(): Unit = {
      dut.io.request.valid.poke(false.B)
      dut.io.trap.poke(true.B)
      edge()
      dut.io.trap.poke(false.B)
      edge()
      dut.io.mode.expect(3.U)
      dut.io.virtualMode.expect(false.B)
      checkTimer()
    }

    def enableState(machine: Boolean = true, supervisor: Boolean = true): Unit = {
      write(0x30c, (BigInt(1) << 63) | (if (machine) BigInt(1) else BigInt(0)))
      write(0x60c, (BigInt(1) << 63) | 1)
      write(0x10c, if (supervisor) 1 else 0)
    }
  }

  it should "store masked independent bank state through all six CSR operations" in run("bank-storage") { d =>
    addresses.foreach(a => assert(d.read(a) == 0))
    val machineBefore = Seq(0x300, 0x304, 0x344).map(a => a -> d.read(a)).toMap
    masks.foreach { case (addr, mask) =>
      d.write(addr, allOnes)
      assert(d.read(addr) == mask)
      d.write(addr, 0)
      assert(d.read(addr) == 0)
      d.access(addr, function = 5, rs1 = 31)
      assert(d.read(addr) == (BigInt(31) & mask))
      d.access(addr, function = 7, rs1 = 17)
      assert(d.read(addr) == (BigInt(14) & mask))
      d.access(addr, function = 6, rs1 = 17)
      assert(d.read(addr) == (BigInt(31) & mask))
      d.access(addr, function = 3, rs1 = 1, source = 18)
      assert(d.read(addr) == (BigInt(13) & mask))
      d.access(addr, function = 2, rs1 = 1, source = BigInt(1) << 63)
      val expected = ((BigInt(1) << 63) | 13) & mask
      assert(d.read(addr) == expected)
      for (function <- Seq(2, 3, 6, 7)) {
        d.access(addr, function = function, rs1 = 0, source = allOnes)
        assert(d.read(addr) == expected)
      }
    }
    d.write(0x041, 3)
    assert(d.read(0x041) == 2)
    d.write(0x005, 3)
    assert(d.read(0x005) == 0)
    d.write(0x044, allOnes)
    assert(d.read(0x044) == 0)
    machineBefore.foreach { case (addr, value) => assert(d.read(addr) == value) }
    d.reset()
    addresses.foreach(a => assert(d.read(a) == 0))
  }

  it should "enforce the complete host state-enable and guest denial truth table" in run("bank-permissions") { d =>
    var rows = 0
    for (machine <- Seq(false, true); supervisor <- Seq(false, true); (name, privilege, virtual) <- modes) {
      d.reset()
      masks.foreach { case (addr, mask) => d.write(addr, BigInt(0x12) & mask) }
      d.enableState(machine, supervisor)
      d.mode(name, privilege, virtual)
      val legal = !virtual && (privilege == 3 || machine && (privilege == 1 || supervisor))
      addresses.foreach { addr =>
        d.read(addr, illegal = !legal)
        d.write(addr, if (addr == 0x800) BigInt(0x1000) else allOnes, illegal = !legal)
      }
      d.machine()
      masks.foreach { case (addr, mask) =>
        assert(d.read(addr) == (if (legal) mask else BigInt(0x12) & mask), s"$name m=$machine s=$supervisor addr=$addr")
      }
      if (!legal) assert(d.read(0x800) == 0)
      assert(d.read(0x044) == 0)
      rows += 1
    }
    assert(rows == 20)
    println(s"UserTimer CSR permissions: truthTableRows=$rows addressesPerRow=${addresses.size} guestException=EX_II")
  }

  it should "leave all added CSR accesses unimplemented in every mode when disabled" in run("bank-disabled", enabled = false) { d =>
    for ((name, privilege, virtual) <- modes) {
      d.reset()
      d.enableState()
      d.mode(name, privilege, virtual)
      addresses.foreach { addr =>
        d.read(addr, illegal = true)
        d.write(addr, allOnes, illegal = true)
      }
    }
  }

  for (enabled <- Seq(false, true)) {
    it should s"preserve existing FP vector supervisor guest and debug CSR state with enabled=$enabled" in
      run(s"existing-csr-${if (enabled) "on" else "off"}", enabled) { d =>
        d.write(0x300, 0x2200)
        d.write(0x003, 0x65)
        assert(d.read(0x003) == 0x65)
        assert(d.read(0x001) == 5)
        assert(d.read(0x002) == 3)
        d.write(0x00f, 7)
        assert(d.read(0x00f) == 7)
        assert(d.read(0x009) == 1)
        assert(d.read(0x00a) == 3)
        d.mode("HS", 1, false)
        d.write(0x140, BigInt("123456789abcdef0", 16))
        assert(d.read(0x140) == BigInt("123456789abcdef0", 16))
        d.machine()
        d.mode("VS", 1, true)
        d.write(0x140, BigInt("fedcba9876543210", 16))
        assert(d.read(0x140) == BigInt("fedcba9876543210", 16))
        d.machine()
        assert(d.read(0x140) == BigInt("123456789abcdef0", 16))
        assert(d.read(0x240) == BigInt("fedcba9876543210", 16))
        d.dut.io.debugTrap.poke(true.B)
        d.edge()
        d.dut.io.debugTrap.poke(false.B)
        d.dut.io.debugMode.expect(true.B)
        d.write(0x7b2, BigInt("cafef00d12345678", 16))
        assert(d.read(0x7b2) == BigInt("cafef00d12345678", 16))
        d.system(BigInt("7b200073", 16))
        d.dut.io.debugMode.expect(false.B)
        d.dut.io.mode.expect(3.U)
      }
  }

  it should "preserve timer deadlines across real read-modify-write transactions and privilege changes" in run("timer-transactions") { d =>
    d.enableState()
    d.mode("HU", 0, false)
    assert(d.read(0x000) == 0)
    assert(d.read(0x004) == 0)
    d.write(0x800, 1)
    assert(d.remaining == 1 && !d.pending)
    d.idle(1)
    assert(d.remaining == 0 && d.pending)
    for (function <- Seq(1, 2, 3, 5, 6, 7)) {
      d.access(0x044, function = function, rs1 = 31, source = allOnes)
      assert(d.pending)
    }
    for (function <- Seq(2, 3, 6, 7)) {
      d.access(0x800, function = function, rs1 = 0, source = allOnes)
      assert(d.pending)
    }
    d.write(0x800, 0)
    assert(!d.pending && d.remaining == 0)
    d.write(0x800, 2)
    d.idle(1)
    assert(d.remaining == 1 && !d.pending)
    d.write(0x800, 7)
    assert(d.remaining == 7 && !d.pending)
    d.idle(7)
    assert(d.pending)
    d.write(0x800, 1)
    d.access(0x800, function = 1, rs1 = 1, source = 2)
    assert(d.remaining == 2 && !d.pending)
    d.idle(2)
    assert(d.pending)

    for (function <- Seq(1, 2, 3, 5, 6, 7)) {
      d.write(0x800, 64)
      d.access(0x800, function = function, rs1 = 5, source = 17)
      d.checkTimer()
    }
    for (function <- Seq(2, 3, 6, 7)) {
      d.write(0x800, 64)
      d.access(0x800, function = function, rs1 = 0, source = allOnes)
      assert(d.remaining == 62)
    }
    for (function <- Seq(2, 3)) {
      d.write(0x800, 64)
      d.access(0x800, function = function, rs1 = 1, source = 0)
      assert(d.remaining == 64)
      d.write(0x800, 1)
      d.idle(1)
      d.access(0x800, function = function, rs1 = 1, source = 0)
      assert(d.remaining == 0 && !d.pending)
    }
    d.write(0x800, 1)
    d.idle(1)
    d.access(0x800, function = 5, rs1 = 0)
    assert(d.remaining == 0 && !d.pending)
    d.access(0x800, function = 1, rs1 = 0, source = 0, rd = 0)
    assert(d.remaining == 0 && !d.pending)
    for (value <- Seq(allOnes, BigInt(1) << 63, (BigInt(1) << 32) + 1)) {
      d.write(0x800, value)
      d.idle(8)
      assert(d.remaining == value - 8)
    }

    d.write(0x800, 128)
    d.machine()
    val frozen = d.remaining
    d.idle(5)
    assert(d.remaining == frozen)
    for ((name, privilege, virtual) <- modes.filterNot(_._1 == "HU")) {
      d.mode(name, privilege, virtual)
      d.idle(5)
      assert(d.remaining == frozen)
      d.machine()
    }
    d.mode("HU", 0, false)
    d.idle(3)
    assert(d.remaining < frozen)
    d.dut.io.debugTrap.poke(true.B)
    d.edge()
    d.dut.io.debugTrap.poke(false.B)
    d.dut.io.debugMode.expect(true.B)
    val debugFrozen = d.remaining
    d.idle(7)
    assert(d.remaining == debugFrozen)
    d.system(BigInt("7b200073", 16))
    d.dut.io.debugMode.expect(false.B)
    d.dut.io.mode.expect(0.U)
    d.idle(3)
    assert(d.remaining < debugFrozen)
    for (uie <- Seq(0, 1); utie <- Seq(0, 0x10)) {
      d.write(0x000, uie)
      d.write(0x004, utie)
      d.write(0x800, 2)
      d.idle(2)
      assert(d.pending, s"timer did not expire with UIE=$uie UTIE=$utie")
    }
    d.write(0x000, 0)
    d.write(0x004, 0)
    assert(d.pending)

    for ((machine, supervisor) <- Seq((false, true), (true, false), (false, false))) {
      d.reset()
      d.enableState(machine, supervisor)
      d.write(0x800, 64)
      d.mode("HU", 0, false)
      val active = d.remaining
      d.read(0x800, illegal = true)
      d.write(0x800, 0, illegal = true)
      assert(d.remaining == active - 4 && !d.pending)
      d.idle(d.remaining.toInt)
      assert(d.pending)
      d.read(0x044, illegal = true)
      d.write(0x044, 0, illegal = true)
      d.write(0x800, 32, illegal = true)
      assert(d.pending && d.remaining == 0)
    }
  }

  it should "bind bank effects to the accepted transaction across bubbles stalls and redirects" in run("timer-transaction-boundaries") { d =>
    d.enableState()
    d.write(0x340, BigInt("13579bdf2468ace0", 16))
    d.access(0x800, function = 1, rs1 = 1, source = 31, idleAddress = Some(0x340))
    assert(d.read(0x340) == BigInt("13579bdf2468ace0", 16))
    assert(d.read(0x800) == 31)
    d.write(0x350, 0x70)
    d.write(0x150, 0x70)
    for (target <- Seq(0x351, 0x151); function <- Seq(1, 2, 3, 5, 6, 7)) {
      d.access(0x040, function = function, rs1 = 3, source = 0x1234,
        idleAddress = Some(target), noAia = true)
    }
    d.access(0x040, idleAddress = Some(0x351), noAia = true)
    d.mode("HU", 0, false)
    d.access(0x800, function = 1, rs1 = 1, source = 40, stalls = 5, idleAddress = Some(0x340))
    assert(d.remaining == 34)
    d.write(0x800, 1)
    d.idle(1)
    assert(d.pending)
    d.access(0x800, function = 2, rs1 = 0, source = allOnes, stalls = 3, idleAddress = Some(0x040))
    assert(d.pending)
    d.access(0x800, function = 1, rs1 = 1, source = 20, flushRequest = true, idleAddress = Some(0x040))
    assert(d.pending)
    d.access(0x800, function = 1, rs1 = 1, source = 20, flushWrite = true, idleAddress = Some(0x040))
    assert(d.pending)
    d.access(0x800, function = 1, rs1 = 1, source = 0, idleAddress = Some(0x340))
    assert(!d.pending)
    d.access(0x040, function = 1, rs1 = 1, source = 0xabc, idleAddress = Some(0x041))
    assert(d.read(0x040) == 0xabc)
    assert(d.read(0x041) == 0)
  }
}
