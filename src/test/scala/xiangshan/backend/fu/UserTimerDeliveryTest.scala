// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.simulator.PeekPokeAPI._
import chisel3.simulator.{ChiselSimulation, ChiselWorkspace, ElaboratedModule}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try
import svsim.{CommonCompilationSettings, Simulation, Workspace}
import svsim.verilator.Backend
import xiangshan.backend.fu.NewCSR.CSREvents.InterruptEventIdentity
import xiangshan.frontend.{BranchPredictionBundle, FtqPtr}

class UserTimerDeliveryTest extends AnyFlatSpec {
  behavior of "User timer production delivery"

  private val configuration = sys.env.getOrElse("UIT04_CONFIGURATION", "enabled")
  require(Set("enabled", "disabled").contains(configuration),
    "UIT04_CONFIGURATION must be enabled or disabled; run each configuration in its own JVM")

  // Reuse only this suite's compiled configurations and their original Chisel port mappings.
  private final class UserTimerCompiledSimulation(runRoot: Path) {
    private case class Compiled(module: ElaboratedModule[UserTimerDeliveryHarness], simulation: Simulation)
    private val compiled = mutable.Map.empty[Boolean, Compiled]

    private def compile(enabled: Boolean)(implicit p: Parameters): Compiled = {
      val path = runRoot.resolve(if (enabled) "compiled-enabled" else "compiled-disabled")
      require(!Files.exists(path), s"Compiled simulation workspace already exists: $path")
      val workspace = new Workspace(path.toString)
      workspace.reset()
      val module = workspace.elaborateGeneratedModule(() => new UserTimerDeliveryHarness)
      workspace.generateAdditionalSources()
      val backend = Backend.initializeFromProcessEnvironment()
      val commonSettings = CommonCompilationSettings(
        availableParallelism = CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
      val backendSettings = Backend.CompilationSettings(
        disabledWarnings = Seq("WIDTH", "STMTDLY"),
        disableFatalExitOnWarnings = true,
        enableAllAssertions = true)
      val simulation = workspace.compile(backend)("verilator", commonSettings, backendSettings, None, false)
      Compiled(module, simulation)
    }

    private def runtimeArtifacts(directory: Path): Vector[Path] = {
      val files = Files.list(directory)
      try {
        files.iterator().asScala.filter { path =>
          val name = path.getFileName.toString
          name == "execution-script.txt" || name == "simulation-log.txt" || name.startsWith("trace.")
        }.toVector
      } finally {
        files.close()
      }
    }

    // Native simulation logs have fixed filenames, so one process and its archival must finish before another starts.
    def run(name: String, enabled: Boolean)(body: UserTimerDeliveryHarness => Unit): Unit = synchronized {
      require(Paths.get("").toRealPath() == runRoot, "Simulation must run from its evidence root")
      implicit val p: Parameters = UserTimerDeliveryParameters(enabled)
      val options = p(xiangshan.DebugOptionsKey)
      utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
      utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
      val scenario = runRoot.resolve(name)
      require(!Files.exists(scenario), s"Scenario evidence directory already exists: $scenario")
      Files.createDirectory(scenario)
      val design = compiled.getOrElseUpdate(enabled, compile(enabled))
      val directory = Paths.get(design.simulation.workingDirectoryPath)
      require(runtimeArtifacts(directory).isEmpty, s"Unarchived simulation evidence exists: $directory")
      Files.write(scenario.resolve("compiled-workspace.txt"),
        s"$directory\n".getBytes(StandardCharsets.UTF_8))

      // Each call starts a new process; only immutable elaboration metadata and the compiled executable are shared.
      val result = Try {
        design.simulation.runElaboratedModule(design.module) { module =>
          body(module.wrapped)
        }
      }
      val archived = Try {
        if (result.isSuccess) {
          Seq("execution-script.txt", "simulation-log.txt").foreach { name =>
            require(Files.isRegularFile(directory.resolve(name)), s"Missing fresh simulation evidence: $name")
          }
        }
        val destination = Files.createDirectory(scenario.resolve("workdir-verilator"))
        runtimeArtifacts(directory).foreach { path =>
          Files.move(path, destination.resolve(path.getFileName))
        }
      }
      result.failed.foreach { error =>
        archived.failed.foreach(error.addSuppressed)
      }
      result.get
      archived.get
    }
  }

  private lazy val compiledSimulation = new UserTimerCompiledSimulation(
    Paths.get(sys.props("uit04.runRoot")).toRealPath())

  private def run(name: String, enabled: Boolean = true)(body: Driver => Unit): Unit = {
    val runRoot = Paths.get(sys.props("uit04.runRoot")).toRealPath()
    assert(Paths.get("").toRealPath() == runRoot)
    compiledSimulation.run(name, enabled) { dut =>
      val driver = new Driver(dut, enabled)
      try {
        driver.reset()
        body(driver)
        println(s"UserTimer delivery PASS: group=$name cycles=${driver.cycles} " +
          s"instructions=${driver.instructions} entries=${driver.entries} consumes=${driver.consumes} " +
          "backend=production ftq=production simulator=verilator")
      } finally {
        Files.write(runRoot.resolve(name).resolve("delivery-events.jsonl"),
          driver.events.mkString("", "\n", "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8))
      }
    }
  }

  private case class FetchIdentity(flag: Boolean, index: BigInt, offset: Int)
  private case class RedirectEvent(cycle: Int, identity: FetchIdentity, target: BigInt,
    iaf: Boolean, ipf: Boolean, igpf: Boolean, fullTarget: BigInt)

  private class Driver(val dut: UserTimerDeliveryHarness, enabled: Boolean) {
    var cycles = 0
    var instructions = 0
    var entries = 0
    var consumes = 0
    var timerWrites = 0
    var reservations = 0
    var requests = 0
    var completions = 0
    var releases = 0
    var acceptedIdentity: Option[String] = None
    var acceptedFetch: Option[FetchIdentity] = None
    var ordinaryPC = BigInt(0x1000)
    val events = mutable.ArrayBuffer.empty[String]
    val committed = mutable.Set.empty[FetchIdentity]
    val backendRedirects = mutable.ArrayBuffer.empty[RedirectEvent]
    val frontendRedirects = mutable.ArrayBuffer.empty[RedirectEvent]
    val fetches = mutable.Set.empty[(Boolean, BigInt)]
    val instructionPCs = mutable.Map.empty[FetchIdentity, BigInt]
    val requestedTimerWrites = mutable.Queue.empty[BigInt]
    val acceptedCSRAddresses = mutable.ArrayBuffer.empty[Int]
    val synchronousTraps = mutable.ArrayBuffer.empty[(Int, BigInt)]
    var lastEntryPC: Option[BigInt] = None
    var qualifiedTicks = BigInt(0)
    var deadline: Option[BigInt] = None
    var expectedPending = false

    def bool(value: Bool): Boolean = value.peek().litToBoolean
    def uint(value: UInt): BigInt = value.peek().litValue
    def zero(data: Data): Unit = data match {
      case b: Bool => b.poke(false.B)
      case u: UInt => u.poke(0.U)
      case s: SInt => s.poke(0.S)
      case vector: Vec[_] => vector.foreach(zero)
      case record: Record => record.elements.values.foreach(zero)
      case other => throw new IllegalArgumentException(s"Unhandled external input type ${other.getClass.getName}")
    }
    def setPointer(ptr: FtqPtr, flag: Boolean, index: BigInt): Unit = {
      ptr.flag.poke(flag.B)
      ptr.value.poke(index.U)
    }
    def record(kind: String, fields: String = ""): Unit =
      events += s"{\"cycle\":$cycles,\"event\":\"$kind\"${if (fields.nonEmpty) "," + fields else ""}}"

    def identity(event: InterruptEventIdentity): String = {
      val interrupt = event.interrupt
      Seq(uint(interrupt.cause), bool(interrupt.debug), bool(interrupt.criticalDebug), bool(interrupt.nmi),
        bool(interrupt.virtualInterruptIsHvictlInject), bool(interrupt.irToHS),
        bool(interrupt.irToVS), bool(interrupt.irToHU), bool(interrupt.isInterrupt),
        uint(interrupt.hvictlIID), bool(event.robIdx.flag), uint(event.robIdx.value),
        bool(event.ftqIdx.flag), uint(event.ftqIdx.value), uint(event.ftqOffset), bool(event.isRVC)).mkString(":")
    }
    def fetchIdentity(event: InterruptEventIdentity): FetchIdentity =
      FetchIdentity(bool(event.ftqIdx.flag), uint(event.ftqIdx.value), uint(event.ftqOffset).toInt)

    def captureRedirect(port: chisel3.util.Valid[xiangshan.Redirect],
      destination: mutable.ArrayBuffer[RedirectEvent], name: String): Unit = {
      if (bool(port.valid)) {
        val bits = port.bits
        val event = RedirectEvent(cycles,
          FetchIdentity(bool(bits.ftqIdx.flag), uint(bits.ftqIdx.value), uint(bits.ftqOffset).toInt),
          uint(bits.cfiUpdate.target), bool(bits.cfiUpdate.backendIAF),
          bool(bits.cfiUpdate.backendIPF), bool(bits.cfiUpdate.backendIGPF), uint(bits.fullTarget))
        destination += event
        record(name, s"\"ftq\":${event.identity.index},\"offset\":${event.identity.offset}," +
          s"\"target\":\"0x${event.target.toString(16)}\"")
      }
    }

    def edge(): Unit = {
      beforeScalarEdge()
      activeWatchdogBoundary.foreach(_.beforeEdge())
      beforePriorityEdge()
      beforeMatrixEdge()
      if (bool(dut.io.accepted.valid) && bool(dut.io.accepted.bits.interrupt.irToHU)) {
        assert(acceptedIdentity.isEmpty, "ROB accepted overlapping HU events")
        acceptedIdentity = Some(identity(dut.io.accepted.bits))
        acceptedFetch = Some(fetchIdentity(dut.io.accepted.bits))
        assert(uint(dut.io.accepted.bits.interrupt.cause) == 4)
        assert(bool(dut.io.accepted.bits.interrupt.isInterrupt))
        record("rob-accepted", s"\"identity\":\"${acceptedIdentity.get}\"")
      }
      if (bool(dut.io.huReserve.valid) && bool(dut.io.huReserveReady)) {
        assert(acceptedIdentity.contains(identity(dut.io.huReserve.bits)))
        reservations += 1
        record("csr-reserve", s"\"identity\":\"${identity(dut.io.huReserve.bits)}\"")
      }
      if (bool(dut.io.huRequestAtControl.valid)) {
        assert(acceptedIdentity.contains(identity(dut.io.huRequestAtControl.bits.event)))
        val pc = instructionPCs(fetchIdentity(dut.io.huRequestAtControl.bits.event))
        dut.io.huRequestAtControl.bits.pc.expect(pc.U)
      }
      if (bool(dut.io.huRequest.valid)) {
        assert(acceptedIdentity.contains(identity(dut.io.huRequest.bits.event)))
        val pc = instructionPCs(fetchIdentity(dut.io.huRequest.bits.event))
        dut.io.huRequest.bits.pc.expect(pc.U)
        if (bool(dut.io.huRequestReady)) {
          requests += 1
          record("csr-request", s"\"pc\":\"0x${pc.toString(16)}\",\"identity\":\"${identity(dut.io.huRequest.bits.event)}\"")
        }
      }
      if (bool(dut.io.huCompletion.valid)) {
        assert(acceptedIdentity.contains(identity(dut.io.huCompletion.bits.event)))
        if (bool(dut.io.huCompletionReady)) {
          completions += 1
          record("csr-completion", s"\"outcome\":${uint(dut.io.huCompletion.bits.outcome)}," +
            s"\"target\":\"0x${uint(dut.io.huCompletion.bits.target.pc).toString(16)}\"")
        }
      }
      if (bool(dut.io.huRelease)) {
        assert(acceptedIdentity.nonEmpty, "Control released an event without ROB ownership")
        releases += 1
        record("event-release")
        acceptedIdentity = None
        acceptedFetch = None
      }
      val tick = uint(dut.io.mode) == 0 && !bool(dut.io.virtualMode) &&
        !bool(dut.io.debugMode) && !bool(dut.io.handler)
      if (enabled) dut.io.timerTick.expect(tick.B)
      if (bool(dut.io.timerWrite.valid)) {
        assert(requestedTimerWrites.nonEmpty, "Timer write has no corresponding software request")
        val load = requestedTimerWrites.dequeue()
        dut.io.timerWrite.bits.expect(load.U)
        deadline = if (load == 0) None else Some(qualifiedTicks + load)
        expectedPending = false
        timerWrites += 1
        record("timer-write", s"\"value\":\"$load\"")
      } else if (bool(dut.io.timerConsume)) {
        assert(bool(dut.io.entryEffect), "Pending consumption requires an actual HU entry effect")
        assert(bool(dut.io.timerPending), "HU entry consumed a timer without pending")
        consumes += 1
        expectedPending = false
        record("timer-consume")
      } else if (enabled && tick) {
        qualifiedTicks += 1
        if (deadline.contains(qualifiedTicks)) expectedPending = true
      }
      if (bool(dut.io.entryEffect)) {
        entries += 1
        record("entry-effect")
      }
      if (bool(dut.io.robFlush.valid)) {
        val bits = dut.io.robFlush.bits
        record("rob-flush", s"\"rob\":${uint(bits.robIdx.value)}," +
          s"\"ftq\":${uint(bits.ftqIdx.value)},\"offset\":${uint(bits.ftqOffset)}")
      }
      if (bool(dut.io.robException.valid)) {
        val pc = architecturalPCResult(uint(dut.io.robException.bits.pc), dut.io.robException.bits.pc.getWidth)
        val exceptionMask = dut.io.robException.bits.exceptionVec.zipWithIndex.foldLeft(BigInt(0)) {
          case (mask, (bit, index)) => if (bool(bit)) mask.setBit(index) else mask
        }
        if (bool(dut.io.robException.bits.isInterrupt)) lastEntryPC = Some(pc)
        else synchronousTraps += ((cycles, pc))
        record("rob-exception", s"\"pc\":\"0x${pc.toString(16)}\",\"interrupt\":${bool(dut.io.robException.bits.isInterrupt)}," +
          s"\"exception_mask\":\"0x${exceptionMask.toString(16)}\",\"trigger\":${uint(dut.io.robException.bits.trigger)}," +
          s"\"single_step\":${bool(dut.io.robException.bits.singleStep)}")
      }
      if (bool(dut.io.csrRequest.valid)) {
        assert(bool(dut.io.csrExeRequest.valid), "A CSR child handshake requires its execution-unit handshake")
        assert(bool(dut.io.csrRequestRob.flag) == bool(dut.io.csrExeRequest.bits.flag) &&
          uint(dut.io.csrRequestRob.value) == uint(dut.io.csrExeRequest.bits.value),
          "CSR child and execution-unit handshakes must carry the same ROB identity")
        val address = uint(dut.io.csrRequest.bits).toInt
        acceptedCSRAddresses += address
        record("software-csr-accepted", s"\"address\":\"0x${address.toHexString}\"")
      }
      dut.io.commits.rob_commits.foreach { port =>
        if (bool(port.valid)) {
          committed += FetchIdentity(bool(port.bits.ftqIdx.flag), uint(port.bits.ftqIdx.value), uint(port.bits.ftqOffset).toInt)
        }
      }
      captureRedirect(dut.io.backendRedirect, backendRedirects, "backend-redirect")
      if (bool(dut.io.frontendRedirect.valid)) {
        val bits = dut.io.frontendRedirect.bits
        val event = RedirectEvent(cycles,
          FetchIdentity(bool(bits.ftqIdx.flag), uint(bits.ftqIdx.value), uint(bits.ftqOffset).toInt),
          uint(bits.cfiUpdate.target), bool(bits.cfiUpdate.backendIAF),
          bool(bits.cfiUpdate.backendIPF), bool(bits.cfiUpdate.backendIGPF), uint(bits.fullTarget))
        frontendRedirects += event
        record("ftq-redirect", s"\"ftq\":${event.identity.index},\"offset\":${event.identity.offset}," +
          s"\"target\":\"0x${event.target.toString(16)}\"")
      }
      if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady)) {
        fetches += ((bool(dut.io.fetchRequest.bits.ftqIdx.flag), uint(dut.io.fetchRequest.bits.ftqIdx.value)))
      }
      dut.clock.step()
      cycles += 1
      if (enabled) {
        val remaining = deadline.map(d => (d - qualifiedTicks).max(BigInt(0))).getOrElse(BigInt(0))
        dut.io.timerRemaining.expect(remaining.U)
        dut.io.timerPending.expect(expectedPending.B)
      }
      afterMatrixEdge()
      afterPriorityEdge()
    }
    def idle(n: Int): Unit = (0 until n).foreach(_ => edge())
    def until(description: String, maximum: Int = 2000)(condition: => Boolean): Unit = {
      var waited = 0
      while (!condition && waited < maximum) { edge(); waited += 1 }
      assert(condition, s"Timed out waiting for $description at cycle $cycles")
    }
    def reset(): Unit = {
      dut.io.instruction.valid.poke(false.B)
      zero(dut.io.instruction.bits)
      dut.io.instructionTail.foreach { lane => lane.valid.poke(false.B); zero(lane.bits) }
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits)
      zero(dut.io.predecode)
      zero(dut.io.interrupts)
      dut.io.fetchReady.poke(true.B)
      dut.io.traceEnable.poke(false.B)
      dut.io.traceStall.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(10)
      dut.reset.poke(false.B)
      idle(400)
      dut.io.mode.expect(3.U)
      dut.io.handler.expect(false.B)
      dut.io.csrSiblingReady.foreach(_.expect(true.B))
    }

    def prediction(stage: BranchPredictionBundle, base: BigInt, flag: Boolean, index: BigInt): Unit = {
      zero(stage)
      stage.pc.foreach(field => field.poke(transportBits(base, field).U))
      stage.valid.foreach(_.poke(true.B))
      setPointer(stage.ftq_idx, flag, index)
    }

    def enqueue(instruction: BigInt, pc: BigInt, compressed: Boolean = false, faultFromFtq: Boolean = false): FetchIdentity = {
      val entryBytes = dut.io.predecode.bits.pd.length * 2
      val base = pc - (pc % entryBytes)
      val offset = ((pc - base) / 2).toInt
      val flag = bool(dut.io.ftqNext.flag)
      val index = uint(dut.io.ftqNext.value)
      val identity = FetchIdentity(flag, index, offset)
      instructionPCs(identity) = pc
      fetches -= ((flag, index))
      committed -= identity
      zero(dut.io.bpu.resp.bits)
      prediction(dut.io.bpu.resp.bits.s1, base, flag, index)
      dut.io.bpu.resp.valid.poke(true.B)
      until("BPU entry ready")(bool(dut.io.bpu.resp.ready))
      edge()
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits.s1)
      prediction(dut.io.bpu.resp.bits.s2, base, flag, index)
      edge()
      zero(dut.io.bpu.resp.bits.s2)
      prediction(dut.io.bpu.resp.bits.s3, base, flag, index)
      edge()
      zero(dut.io.bpu.resp.bits.s3)
      until("actual FTQ request")(fetches.contains((flag, index)))
      val fetchedFaultKind = if (faultFromFtq) {
        val actual = activePriorityMonitor.get
        until("actual target-fetch fault class") {
          actual.actualIcacheFaults((flag, index)) && actual.actualPrefetchFaults.contains((flag, index))
        }
        Some(actual.actualPrefetchFaults((flag, index)))
      } else None
      zero(dut.io.predecode.bits)
      setPointer(dut.io.predecode.bits.ftqIdx, flag, index)
      dut.io.predecode.bits.pc.zipWithIndex.foreach { case (value, i) => value.poke(transportBits(base + 2 * i, value).U) }
      dut.io.predecode.bits.pd(offset).valid.poke(true.B)
      dut.io.predecode.bits.pd(offset).isRVC.poke(compressed.B)
      dut.io.predecode.bits.instrRange(offset).poke(true.B)
      dut.io.predecode.bits.ftqOffset.poke(offset.U)
      dut.io.predecode.bits.target.poke(transportBits(pc + (if (compressed) 2 else 4), dut.io.predecode.bits.target).U)
      dut.io.predecode.valid.poke(true.B)
      edge()
      dut.io.predecode.valid.poke(false.B)
      zero(dut.io.instruction.bits)
      dut.io.instruction.bits.instr.poke(instruction.U)
      // The frontend encodes no trigger as 15; zero requests a breakpoint exception.
      dut.io.instruction.bits.trigger.poke(xiangshan.TriggerAction.None)
      dut.io.instruction.bits.pc.poke(transportBits(pc, dut.io.instruction.bits.pc).U)
      fetchedFaultKind.foreach { kind =>
        dut.io.instruction.bits.exceptionVec(xiangshan.ExceptionNO.EX_IAF).poke((kind == 3).B)
        dut.io.instruction.bits.exceptionVec(xiangshan.ExceptionNO.EX_IPF).poke((kind == 1).B)
        dut.io.instruction.bits.exceptionVec(xiangshan.ExceptionNO.EX_IGPF).poke((kind == 2).B)
        dut.io.instruction.bits.backendException.poke(true.B)
      }
      dut.io.instruction.bits.pd.valid.poke(true.B)
      dut.io.instruction.bits.pd.isRVC.poke(compressed.B)
      setPointer(dut.io.instruction.bits.ftqPtr, flag, index)
      dut.io.instruction.bits.ftqOffset.poke(offset.U)
      dut.io.instruction.bits.isLastInFtqEntry.poke(true.B)
      dut.io.instruction.valid.poke(true.B)
      until("frontend instruction acceptance")(bool(dut.io.instruction.ready))
      edge()
      dut.io.instruction.valid.poke(false.B)
      // Invalid-cycle inputs may change immediately; precise recovery must use the saved FTQ transaction.
      zero(dut.io.instruction.bits)
      dut.io.instruction.bits.pc.poke(0x3ffe.U)
      setPointer(dut.io.instruction.bits.ftqPtr, !flag, 0)
      instructions += 1
      record("instruction", s"\"pc\":\"0x${pc.toString(16)}\",\"bits\":\"0x${instruction.toString(16)}\"," +
        s"\"ftq\":$index,\"offset\":$offset")
      identity
    }

    def execute(instruction: BigInt): Unit = {
      val trapsBefore = synchronousTraps.size
      val redirectsBefore = backendRedirects.size
      val identity = enqueue(instruction, ordinaryPC)
      ordinaryPC += 4
      until("instruction commit or retirement redirect") {
        committed(identity) || backendRedirects.drop(redirectsBefore).exists(_.identity == identity)
      }
      idle(12)
      assert(synchronousTraps.size == trapsBefore,
        s"Instruction 0x${instruction.toString(16)} raised an unexpected ordinary trap: ${synchronousTraps.drop(trapsBefore)}")
    }
    def addi(rd: Int, rs: Int, immediate: Int): BigInt =
      (BigInt(immediate & 0xfff) << 20) | (BigInt(rs) << 15) | (BigInt(rd) << 7) | 0x13
    def loadX1(value: BigInt): Unit = {
      require(value >= 0 && value < (BigInt(1) << 31))
      val high = (value + 0x800) >> 12
      val low = (value - (high << 12)).toInt
      if (high != 0) {
        execute((high << 12) | (BigInt(1) << 7) | 0x37)
        if (low != 0) execute(addi(1, 1, low))
      } else execute(addi(1, 0, low))
    }
    def write(address: Int, value: BigInt): Unit = {
      loadX1(value)
      val acceptedBefore = acceptedCSRAddresses.size
      if (address == 0x800) requestedTimerWrites.enqueue(value)
      execute((BigInt(address) << 20) | (BigInt(1) << 15) | (BigInt(1) << 12) | 0x73)
      assert(acceptedCSRAddresses.drop(acceptedBefore).toSeq == Seq(address),
        s"CSR 0x${address.toHexString} was not accepted exactly once")
      if (address == 0x800) assert(requestedTimerWrites.isEmpty, "Accepted software timer write did not reach UserTimer")
    }
    private case class CandidateSample(cycle: Int, filterMask: Int, robHU: Boolean, kill: Boolean)
    private case class CancelOperation(name: String, address: Int, immediate: Int, delayedIndex: Int)
    private var matrixObservationsEnabled = false
    private var previousCandidate: Option[CandidateSample] = None
    private var sampledCandidate: Option[CandidateSample] = None
    private var operationUnderTest: Option[CancelOperation] = None
    private var operationC0: Option[Int] = None
    private var operationC1: Option[Int] = None
    private var operationLeading: Option[Int] = None
    private var reloadC1: Option[Int] = None
    private var traceStallAtProposedEffect = false
    private var traceStallRemaining = 0
    private var traceAdmissionWaiting = false
    private var traceDeferredCycles = 0
    private var traceDeferralBank = Seq.empty[BigInt]
    private var traceDeferralCounts = (0, 0, 0)
    private var traceEffectCycle: Option[Int] = None
    private var traceHolding: Option[(String, BigInt, BigInt)] = None
    private val encodedHU = mutable.ArrayBuffer.empty[(Int, FetchIdentity)]
    private var returns = 0
    private var nmiReservations = 0
    private var nmiEntries = 0
    private var nmiFetch: Option[FetchIdentity] = None
    private var traceCollisionActive = false
    private var nmiTraceInputs = 0
    private var traceOverwriteObserved = false

    private def huStage(port: chisel3.util.Valid[xiangshan.backend.fu.NewCSR.CSREvents.InterruptDescriptor]): Boolean =
      bool(port.valid) && bool(port.bits.irToHU)

    private def currentCandidate: CandidateSample = CandidateSample(cycles,
      dut.io.filterCandidateStages.zipWithIndex.foldLeft(0) {
        case (mask, (port, index)) => if (huStage(port)) mask | (1 << index) else mask
      }, huStage(dut.io.robCandidate), bool(dut.io.candidateKill))

    private def beforeMatrixEdge(): Unit = {
      if (!matrixObservationsEnabled) return
      // Raise the normal encoder stall before the proposed completion edge.
      // Capacity admission must withdraw E until this finite stall has drained.
      if (traceStallAtProposedEffect && bool(dut.io.entryEffect)) {
        assert(!bool(dut.io.traceBlocked))
        traceDeferralBank = Seq(dut.io.ustatus, dut.io.uepc, dut.io.ucause, dut.io.utval).map(uint)
        traceDeferralCounts = (entries, consumes, completions)
        dut.io.traceStall.poke(true.B)
        traceStallAtProposedEffect = false
        traceStallRemaining = 8
        traceAdmissionWaiting = true
        dut.io.entryEffect.expect(false.B)
        dut.io.huCompletionReady.expect(false.B)
        record("trace-stall-defers-proposed-effect")
      }
      if (traceAdmissionWaiting) {
        assert((entries, consumes, completions) == traceDeferralCounts,
          "HU effects must remain unchanged until deferred admission resumes")
        assert(Seq(dut.io.ustatus, dut.io.uepc, dut.io.ucause, dut.io.utval).map(uint) == traceDeferralBank,
          "HU bank changed before deferred admission resumed")
        dut.io.handler.expect(false.B)
        dut.io.timerPending.expect(true.B)
        dut.io.huCompletion.valid.expect(true.B)
        if (bool(dut.io.entryEffect)) {
          assert(traceStallRemaining == 0 && !bool(dut.io.traceStall))
          assert(traceDeferredCycles >= 8, "The finite trace stall did not defer HU admission")
          traceAdmissionWaiting = false
          record("trace-admission-resumed", s"\"deferred_cycles\":$traceDeferredCycles")
        } else {
          dut.io.timerConsume.expect(false.B)
          dut.io.huCompletionReady.expect(false.B)
          traceDeferredCycles += 1
        }
      }
      if (bool(dut.io.entryEffect)) traceEffectCycle = Some(cycles)
      val sample = currentCandidate
      sampledCandidate = Some(sample)
      dut.io.filterCandidateStages.foreach { port =>
        if (huStage(port)) {
          port.bits.cause.expect(4.U)
          port.bits.isInterrupt.expect(true.B)
          port.bits.irToHS.expect(false.B)
          port.bits.irToVS.expect(false.B)
          port.bits.nmi.expect(false.B)
          port.bits.debug.expect(false.B)
        }
      }
      if (sample.kill) {
        assert(!huStage(dut.io.candidate), "Revocation must mask Filter output in the same cycle")
        assert(!sample.robHU, "Revocation must mask the ROB candidate in the same cycle")
      }
      operationUnderTest.foreach { operation =>
        if (operationC0.isEmpty && timerWrites >= 1 && bool(dut.io.legalCSRWrite.valid) &&
          uint(dut.io.legalCSRWrite.bits) == operation.address) {
          operationC0 = Some(cycles)
          assert(sample.kill)
          assert(bool(dut.io.robHeadValid) && !bool(dut.io.robHeadInterruptSafe),
            "The cancellation CSR must use the real unsafe ROB boundary")
          // intrBitSetReg includes same-cycle revocation; use its last unrevoked
          // observation to prove an already held ROB candidate was invalidated.
          val robWasHeld = previousCandidate.exists(s => s.robHU && !s.kill)
          val occupied = (0 until 6).filter(i => (sample.filterMask & (1 << i)) != 0) ++
            (if (robWasHeld) Seq(6) else Seq.empty[Int])
          operationLeading = occupied.lastOption
          record("candidate-cancel-c0", s"\"operation\":\"${operation.name}\",\"filter_mask\":${sample.filterMask}," +
            s"\"rob_previously_held\":$robWasHeld,\"leading\":${operationLeading.getOrElse(-1)}")
        }
        val delayed = dut.io.userQualifierWrites(operation.delayedIndex)
        if (operationC0.nonEmpty && operationC1.isEmpty && bool(delayed.valid)) {
          assert(cycles == operationC0.get + 1, "The accepted CSR write and delayed effect must remain paired")
          delayed.bits.expect(operation.immediate.U)
          operationC1 = Some(cycles)
          assert(sample.filterMask == 0 && !sample.robHU, "C0 must already have removed every old HU stage")
          assert(sample.kill, "C1 must continue to revoke stale candidates")
          if (operation.address == 0x800 && operation.immediate == 1) reloadC1 = Some(cycles)
          record("candidate-cancel-c1", s"\"operation\":\"${operation.name}\"")
        }
      }
      reloadC1.foreach { writeCycle =>
        for (stage <- 0 until 6 if (sample.filterMask & (1 << stage)) != 0) {
          assert(cycles >= writeCycle + 3 + stage,
            s"Reloaded pending resurrected an old Filter stage $stage")
        }
        if (sample.robHU) assert(cycles >= writeCycle + 9,
          "Reloaded pending reached ROB without traversing all six Filter stages")
      }
      if (bool(dut.io.returnEffect)) { returns += 1; record("user-return-effect") }
      if (bool(dut.io.accepted.valid) && bool(dut.io.accepted.bits.interrupt.nmi) &&
        !bool(dut.io.accepted.bits.interrupt.debug) && !bool(dut.io.accepted.bits.interrupt.irToHU)) {
        nmiReservations += 1
        nmiFetch = Some(fetchIdentity(dut.io.accepted.bits))
        record("nmi-accepted", s"\"identity\":\"${identity(dut.io.accepted.bits)}\"")
      }
      if (bool(dut.io.mnEntry)) { nmiEntries += 1; record("nmi-entry-effect") }
      val traceInputs = dut.io.traceInput.blocks.filter(b => bool(b.valid) && b.bits.huTimer.exists(bool))
      assert(traceInputs.nonEmpty == bool(dut.io.entryEffect), "HU trace insertion must coincide with E")
      if (traceInputs.nonEmpty) {
        assert(traceInputs.size == 1 && !bool(dut.io.traceBlocked))
        val b = traceInputs.head.bits
        val id = FetchIdentity(bool(b.ftqIdx.get.flag), uint(b.ftqIdx.get.value), uint(b.ftqOffset.get).toInt)
        assert(acceptedFetch.contains(id), "HU trace insertion uses the wrong accepted identity")
      }
      val held = dut.io.traceHeld.blocks.filter(b => bool(b.valid) && b.bits.huTimer.exists(bool))
      if (traceCollisionActive) {
        dut.io.traceInput.blocks.foreach { block =>
          if (bool(block.valid) && !block.bits.huTimer.exists(bool)) {
            val b = block.bits
            val id = FetchIdentity(bool(b.ftqIdx.get.flag), uint(b.ftqIdx.get.value), uint(b.ftqOffset.get).toInt)
            if (nmiFetch.contains(id) && uint(b.tracePipe.itype) == 2) {
              assert(bool(dut.io.traceBlocked), "The collision must reach a genuinely blocked trace input")
              nmiTraceInputs += 1
              record("nmi-trace-input-while-blocked", s"\"ftq\":${id.index},\"offset\":${id.offset}")
            }
          }
        }
        if (traceHolding.nonEmpty && held.isEmpty && bool(dut.io.traceBlocked) && nmiTraceInputs > 0 &&
          dut.io.traceHeld.blocks.exists(b => bool(b.valid))) {
          if (!traceOverwriteObserved) record("held-hu-trace-overwritten")
          traceOverwriteObserved = true
        }
      }
      assert(held.size <= 1)
      if (held.nonEmpty && bool(dut.io.traceBlocked)) {
        val b = held.head.bits
        val stable = (s"${bool(b.ftqIdx.get.flag)}:${uint(b.ftqIdx.get.value)}:${uint(b.ftqOffset.get)}",
          uint(b.tracePipe.itype), uint(b.tracePipe.ilastsize))
        traceHolding.foreach(saved => assert(stable == saved, "HU trace identity changed while held"))
        traceHolding = Some(stable)
        assert(stable._2 == 2 && uint(b.tracePipe.iretire) == 0)
      }
      dut.io.traceEncoder.blocks.zipWithIndex.foreach { case (block, index) =>
        if (bool(block.valid) && block.bits.huTimer.exists(bool)) {
          assert(entries >= 1, "HU trace escaped before its entry effect")
          val identity = FetchIdentity(bool(block.bits.ftqIdx.get.flag),
            uint(block.bits.ftqIdx.get.value), uint(block.bits.ftqOffset.get).toInt)
          assert(!encodedHU.exists(_._2 == identity), "HU trace was delivered more than once")
          val expectedPC = instructionPCs(identity)
          val actualPC = architecturalPCResult(uint(dut.io.traceAddresses(index)) + 2 * uint(dut.io.traceOffsets(index)), dut.io.traceAddresses(index).getWidth)
          assert(actualPC == expectedPC, "HU trace address does not belong to the accepted instruction")
          dut.io.traceCause.expect(((BigInt(1) << 63) | 4).U)
          dut.io.traceTval.expect(0.U)
          dut.io.tracePrivilege.expect(0.U)
          block.bits.tracePipe.itype.expect(2.U)
          encodedHU += ((cycles, identity))
          record("hu-trace-encoder", s"\"pc\":\"0x${expectedPC.toString(16)}\",\"ftq\":${identity.index}," +
            s"\"offset\":${identity.offset},\"cause\":\"0x8000000000000004\",\"tval\":0")
        }
      }
    }

    private def afterMatrixEdge(): Unit = {
      if (!matrixObservationsEnabled) return
      sampledCandidate.foreach { before =>
        if (before.kill) {
          val after = currentCandidate
          assert(after.filterMask == 0 && !after.robHU,
            "Revocation must clear all six Filter registers and the ROB pre-acceptance stage")
        }
        previousCandidate = Some(before)
      }
      if (traceStallRemaining > 0) {
        traceStallRemaining -= 1
        if (traceStallRemaining == 0) {
          dut.io.traceStall.poke(false.B)
          record("trace-stall-release")
        }
      }
    }

    def finishMatrixScenario(): Unit = {
      assert(acceptedIdentity.isEmpty && reservations == releases)
      assert(requestedTimerWrites.isEmpty)
      record("scenario-end", s"\"entries\":$entries,\"consumes\":$consumes,\"reservations\":$reservations," +
        s"\"requests\":$requests,\"completions\":$completions,\"releases\":$releases,\"returns\":$returns," +
        s"\"nmi_reservations\":$nmiReservations,\"nmi_entries\":$nmiEntries,\"trace_deferred_cycles\":$traceDeferredCycles")
    }

    private def resetMatrixScenario(name: String): Unit = {
      // Never hide a reset-canceled accepted event in the scenario accounting.
      finishMatrixScenario()
      activePriorityMonitor = None
      fullAddressOracle = false
      matrixObservationsEnabled = false
      entries = 0; consumes = 0; timerWrites = 0
      reservations = 0; requests = 0; completions = 0; releases = 0; returns = 0
      acceptedIdentity = None; acceptedFetch = None; lastEntryPC = None
      qualifiedTicks = 0; deadline = None; expectedPending = false
      committed.clear(); backendRedirects.clear(); frontendRedirects.clear(); fetches.clear()
      instructionPCs.clear(); requestedTimerWrites.clear(); acceptedCSRAddresses.clear(); synchronousTraps.clear()
      previousCandidate = None; sampledCandidate = None
      operationUnderTest = None; operationC0 = None; operationC1 = None; operationLeading = None; reloadC1 = None
      traceStallAtProposedEffect = false; traceStallRemaining = 0
      traceAdmissionWaiting = false; traceDeferredCycles = 0; traceDeferralBank = Seq.empty
      traceDeferralCounts = (0, 0, 0); traceEffectCycle = None
      traceHolding = None; encodedHU.clear()
      nmiReservations = 0; nmiEntries = 0; nmiFetch = None
      traceCollisionActive = false; nmiTraceInputs = 0; traceOverwriteObserved = false
      ordinaryPC = 0x1000
      record("scenario-start", s"\"name\":\"$name\"")
      reset()
    }

    private def nextInstructionPC(): BigInt = {
      val pc = ordinaryPC
      ordinaryPC += 4
      pc
    }

    private def csrWriteX1(address: Int): BigInt =
      (BigInt(address) << 20) | (BigInt(1) << 15) | (BigInt(1) << 12) | 0x73

    private def csrWriteImmediate(address: Int, immediate: Int): BigInt = {
      require(immediate >= 0 && immediate < 32)
      (BigInt(address) << 20) | (BigInt(immediate) << 15) | (BigInt(5) << 12) | 0x73
    }

    private def cancellationTrial(operation: CancelOperation, count: Int, gap: Int): Option[Int] = {
      resetMatrixScenario(s"${operation.name}-n$count-gap$gap")
      enterUser()
      loadX1(count)
      matrixObservationsEnabled = true
      operationUnderTest = Some(operation)
      requestedTimerWrites.enqueue(count)
      val timer = enqueue(csrWriteX1(0x800), nextInstructionPC())
      // Only actual CSR instructions or an empty ROB can precede cancellation.
      // The cancellation value is encoded in zimm, so no safe ADDI can take HU first.
      idle(gap)
      if (operation.address == 0x800) requestedTimerWrites.enqueue(operation.immediate)
      val cancel = enqueue(csrWriteImmediate(operation.address, operation.immediate), nextInstructionPC())
      until("real C0/C1 cancellation and both CSR retirements") {
        operationC1.nonEmpty && committed(timer) && committed(cancel)
      }
      idle(count + 16)
      assert(requestedTimerWrites.isEmpty && reservations == 0 && entries == 0 && consumes == 0)
      assert(synchronousTraps.isEmpty)
      if (operation.address == 0x800 && operation.immediate == 0) {
        dut.io.timerRemaining.expect(0.U); dut.io.timerPending.expect(false.B)
        assert(currentCandidate.filterMask == 0 && !currentCandidate.robHU)
      } else if (operation.address == 0x800 || operation.immediate != 0) {
        dut.io.timerPending.expect(true.B)
        until("new fully delayed candidate")(currentCandidate.robHU)
      } else {
        dut.io.timerRemaining.expect(0.U); dut.io.timerPending.expect(true.B)
        assert(currentCandidate.filterMask == 0 && !currentCandidate.robHU)
      }
      operationLeading
    }

    def candidateCancellationMatrix(): Unit = {
      val operations = Seq(
        CancelOperation("timer-zero", 0x800, 0, 2),
        CancelOperation("timer-reload-one", 0x800, 1, 2),
        CancelOperation("uie-mask", 0x000, 0, 0),
        CancelOperation("utie-mask", 0x004, 0, 1),
        CancelOperation("uie-same-value", 0x000, 1, 0))
      operations.foreach { operation =>
        val covered = mutable.Set.empty[Int]
        // This finite search changes only real timer programming and frontend timing.
        // Coverage is derived from observed stage occupancy, never from requested N.
        for (gap <- Seq(0, 8); count <- 1 to 24 if covered.size < 7) {
          cancellationTrial(operation, count, gap).foreach(covered += _)
        }
        assert(covered.toSet == (0 until 7).toSet,
          s"Uncovered leading stages for ${operation.name}: ${(0 until 7).filterNot(covered)}")
        record("cancel-stage-coverage", s"\"operation\":\"${operation.name}\",\"stages\":[${covered.toSeq.sorted.mkString(",")}]")
      }
    }

    private def awaitAutomaticEntry(pc: BigInt, expectedEntries: Int): FetchIdentity = {
      until("pending and ROB candidate")(bool(dut.io.timerPending) && currentCandidate.robHU)
      val id = enqueue(0x13, pc, compressed = true)
      until("HU effect, FTQ target, and release") {
        entries == expectedEntries && releases == expectedEntries &&
          frontendRedirects.exists(r => r.identity == id && r.target == 0x2000)
      }
      assert(!committed(id))
      dut.io.uepc.expect(pc.U)
      dut.io.ucause.expect(((BigInt(1) << 63) | 4).U)
      dut.io.utval.expect(0.U)
      assert(consumes == expectedEntries)
      id
    }

    def automaticReloadReturn(): Unit = {
      resetMatrixScenario("automatic-handler-reload-uret-second-entry")
      enterUser()
      matrixObservationsEnabled = true
      write(0x800, 1)
      val pc = BigInt(0x1236)
      val first = awaitAutomaticEntry(pc, 1)
      ordinaryPC = 0x2000
      write(0x800, 1)
      idle(16)
      dut.io.handler.expect(true.B)
      dut.io.timerRemaining.expect(1.U)
      dut.io.timerPending.expect(false.B)
      assert(entries == 1 && consumes == 1 && returns == 0)
      assert(currentCandidate.filterMask == 0 && !currentCandidate.robHU)
      execute(BigInt("00200073", 16))
      assert(returns == 1)
      dut.io.handler.expect(false.B)
      val second = awaitAutomaticEntry(pc, 2)
      assert(first != second, "The second entry must carry a new FTQ transaction")
      idle(24)
      assert(entries == 2 && consumes == 2 && reservations == 2 && requests == 2 && completions == 2 && releases == 2)
      assert(returns == 1 && synchronousTraps.isEmpty)
    }

    def traceAfterEntryMatrix(): Unit = {
      for (deferAdmission <- Seq(false, true)) {
        resetMatrixScenario(s"trace-stall-defer-admission-$deferAdmission")
        dut.io.traceEnable.poke(true.B)
        enterUser()
        write(0x800, 1)
        matrixObservationsEnabled = true
        until("pending and ROB candidate")(bool(dut.io.timerPending) && currentCandidate.robHU)
        traceStallAtProposedEffect = deferAdmission
        val id = enqueue(0x13, 0x1276, compressed = true)
        until("entry effect")(entries == 1)
        if (!deferAdmission) {
          assert(traceEffectCycle.contains(cycles - 1))
          dut.io.traceStall.poke(true.B)
          traceStallRemaining = 8
          record("trace-stall-after-effect")
        }
        until("HU trace and FTQ delivery") {
          encodedHU.count(_._2 == id) == 1 && releases == 1 && traceStallRemaining == 0 &&
            frontendRedirects.exists(r => r.identity == id && r.target == 0x2000)
        }
        idle(24)
        if (deferAdmission) assert(traceDeferredCycles >= 8 && !traceAdmissionWaiting,
          "Trace capacity admission did not defer and then complete HU entry")
        else assert(traceDeferredCycles == 0)
        assert(encodedHU.count(_._2 == id) == 1)
        assert(!traceOverwriteObserved)
        assert(entries == 1 && consumes == 1 && completions == 1 && releases == 1)
        assert(!committed(id) && synchronousTraps.isEmpty)
      }
    }

    def traceCollisionWithNmi(): Unit = {
      resetMatrixScenario("trace-admission-deferral-then-real-nmi")
      write(0x305, 0x3000)
      enterUser()
      dut.io.traceEnable.poke(true.B)
      write(0x800, 1)
      matrixObservationsEnabled = true
      until("pending and ROB candidate")(bool(dut.io.timerPending) && currentCandidate.robHU)
      traceCollisionActive = true
      traceStallAtProposedEffect = true
      val userPC = BigInt(0x12b6)
      val user = enqueue(0x13, userPC, compressed = true)
      until("HU entry after finite trace admission deferral")(entries == 1)
      assert(traceDeferredCycles >= 8 && !traceAdmissionWaiting)
      assert(traceEffectCycle.contains(cycles - 1))
      // Stall again at E+1. The admitted HU record must already have downstream
      // capacity, even when a later legacy NMI produces another trace input.
      dut.io.traceStall.poke(true.B)
      traceStallRemaining = 0
      record("trace-stall-after-admitted-effect", s"\"effect_cycle\":${traceEffectCycle.get}")
      until("HU target release during the later trace stall") {
        releases == 1 && bool(dut.io.traceBlocked) &&
          frontendRedirects.exists(r => r.identity == user && r.target == 0x2000)
      }
      assert(encodedHU.count(_._2 == user) <= 1 && !traceOverwriteObserved)
      dut.io.traceStall.expect(true.B)
      dut.io.interrupts.nmi.nmi_31.poke(true.B)
      record("one-cycle-nmi-input", "\"cause\":31")
      edge()
      dut.io.interrupts.nmi.nmi_31.poke(false.B)
      // Backend registers external interrupts before the CSR pending register.
      until("one-cycle NMI latched by the real CSR", maximum = 32)(uint(dut.io.mnPending).testBit(31))
      dut.io.interrupts.nmi.nmi_31.expect(false.B)
      until("real NMI descriptor at ROB") {
        bool(dut.io.robCandidate.valid) && bool(dut.io.robCandidate.bits.nmi) &&
          uint(dut.io.robCandidate.bits.cause) == 31
      }
      val handlerPC = BigInt(0x2006)
      val handler = enqueue(0x13, handlerPC, compressed = true)
      until("real NMI entry and competing trace input") {
        nmiReservations == 1 && nmiEntries == 1 && nmiTraceInputs == 1 &&
          frontendRedirects.exists(r => r.identity == handler && r.target == 0x3000)
      }
      dut.io.mnepc.expect(handlerPC.U)
      dut.io.mncause.expect(((BigInt(1) << 63) | 31).U)
      dut.io.uepc.expect(userPC.U)
      dut.io.handler.expect(true.B)
      assert(!uint(dut.io.mnPending).testBit(31))
      assert(!committed(user) && !committed(handler))
      dut.io.traceStall.poke(false.B)
      record("trace-collision-unblock")
      idle(32)
      finishMatrixScenario()
      record("trace-collision-result", s"\"hu_trace_count\":${encodedHU.count(_._2 == user)}," +
        s"\"overwrite_observed\":$traceOverwriteObserved,\"nmi_trace_inputs\":$nmiTraceInputs")
      assert(encodedHU.count(_._2 == user) == 1,
        "A later real NMI trace must preserve the original admitted HU trace")
      assert(!traceOverwriteObserved, "A later trace overwrote the admitted HU record")
      assert(entries == 1 && consumes == 1 && completions == 1 && releases == 1)
      assert(nmiReservations == 1 && nmiEntries == 1 && synchronousTraps.isEmpty)
    }

    import freechips.rocketchip.rocket.Instructions
    import chisel3.util.BitPat

    // Check encodings against the repository masks before issuing real instructions.
    def checkedInstruction(bits: BigInt, pattern: BitPat): BigInt = {
      require((bits & pattern.mask) == pattern.value)
      bits
    }
    def vsetFullE8M8: BigInt =
      checkedInstruction(BigInt("00307157", 16), Instructions.VSETVLI)
    def vmvVi(vd: Int, immediate: Int): BigInt = {
      require(vd >= 0 && vd < 32 && immediate >= -16 && immediate < 16)
      checkedInstruction((BigInt(0x17) << 26) | (BigInt(1) << 25) |
        (BigInt(immediate & 31) << 15) | (BigInt(3) << 12) | (BigInt(vd) << 7) | 0x57,
        Instructions.VMV_V_I)
    }
    def vectorVV(saturating: Boolean, vd: Int = 24, vs2: Int = 16, vs1: Int = 8): BigInt =
      checkedInstruction((BigInt(if (saturating) 0x20 else 0) << 26) |
        (BigInt(1) << 25) | (BigInt(vs2) << 20) | (BigInt(vs1) << 15) |
        (BigInt(vd) << 7) | 0x57,
        if (saturating) Instructions.VSADDU_VV else Instructions.VADD_VV)
    def vmvXs(rd: Int, vs2: Int): BigInt =
      checkedInstruction((BigInt(0x10) << 26) | (BigInt(1) << 25) |
        (BigInt(vs2) << 20) | (BigInt(2) << 12) | (BigInt(rd) << 7) | 0x57,
        Instructions.VMV_X_S)
    def csrReadIntoX3(address: Int): BigInt =
      (BigInt(address) << 20) | (BigInt(2) << 12) | (BigInt(3) << 7) | 0x73
    def x3ToUserScratch: BigInt =
      (BigInt(0x040) << 20) | (BigInt(3) << 15) | (BigInt(1) << 12) | 0x73
    def slli(rd: Int, rs: Int, amount: Int): BigInt = {
      require(amount >= 0 && amount < 64)
      checkedInstruction((BigInt(amount) << 20) | (BigInt(rs) << 15) |
        (BigInt(1) << 12) | (BigInt(rd) << 7) | 0x13, Instructions.SLLI)
    }
    def setCSRBit(address: Int, bit: Int): Unit = {
      require(bit >= 0 && bit < 64)
      execute(addi(1, 0, 1))
      if (bit != 0) execute(slli(1, 1, bit))
      execute((BigInt(address) << 20) | (BigInt(1) << 15) | (BigInt(2) << 12) | 0x73)
    }





    var activePriorityMonitor: Option[PriorityMonitor] = None
    def beforePriorityEdge(): Unit = activePriorityMonitor.foreach(_.beforeEdge())
    def afterPriorityEdge(): Unit = activePriorityMonitor.foreach(_.afterEdge())

    private type RobIdentity = (Boolean, BigInt)
    private def robIdentity(ptr: xiangshan.backend.rob.RobPtr): RobIdentity =
      (bool(ptr.flag), uint(ptr.value))

    // Follow every accepted uop and writeback of the same original instruction.
    class PriorityMonitor {
      var machineEntries = 0
      var hostEntries = 0
      var guestEntries = 0
      var nmiEntries = 0
      var debugEntries = 0
      var acceptedNmis = 0
      var observedIllegalSafeHead = false
      val actualIcacheFaults = mutable.Set.empty[(Boolean, BigInt)]
      val actualPrefetchFaults = mutable.Map.empty[(Boolean, BigInt), Int]

      private var targetPC: Option[BigInt] = None
      private var targetRob: Option[RobIdentity] = None
      private var vectorMode = false
      private var expectedSaturation = false
      private var expectedByte = 0
      private var unsealedPendingSeen = false
      private var actualRetires = 0
      private var firstCycle: Option[Int] = None
      private var lastCycle: Option[Int] = None
      private var acceptCycle: Option[Int] = None
      private var lastWbCycle = -1
      private val enqueued = mutable.ArrayBuffer.empty[(Int, Int, Boolean, Boolean)]
      private val issued = mutable.Set.empty[Int]
      private val writtenBack = mutable.Set.empty[Int]
      private var nmiAtReserve: Option[Int] = None
      private var clearNmiAfterEdge = false
      private var holdWithoutNmi = false
      // Backend registers external NMI once before CSR source-priority acknowledgement.
      private var nmi31HoldStart: Option[Int] = None
      private var checkNmi31AfterAck = false
      var nmi31RetainedAtAck = false

      def armVector(pc: BigInt, saturating: Boolean, resultByte: Int): Unit = {
        require(targetPC.isEmpty)
        targetPC = Some(pc)
        vectorMode = true
        expectedSaturation = saturating
        expectedByte = resultByte
      }
      def armIllegal(pc: BigInt): Unit = {
        require(targetPC.isEmpty)
        targetPC = Some(pc)
      }
      def holdAndPulseNmiOnNextReserve(cause: Int): Unit = {
        require(cause == 31 || cause == 43)
        require(nmiAtReserve.isEmpty)
        nmiAtReserve = Some(cause)
      }

      def holdWithoutNmiOnNextReserve(): Unit = { holdWithoutNmi = true }

      def holdNmi31AcrossEntry(): Unit = {
        require(nmiEntries == 0 && acceptedNmis == 0 && nmi31HoldStart.isEmpty && !clearNmiAfterEdge)
        dut.io.interrupts.nmi.nmi_31.expect(false.B)
        assert(uint(dut.io.mnPending).testBit(31))
        nmi31HoldStart = Some(cycles)
        dut.io.interrupts.nmi.nmi_31.poke(true.B)
        record("nmi31-source-held-before-handler-head")
      }

      def beforeEdge(): Unit = {
        nmi31HoldStart.foreach { start =>
          dut.io.interrupts.nmi.nmi_31.expect(true.B)
          if (bool(dut.io.mnEntry)) {
            assert(cycles > start, "The source must reach CSR through Backend's register before acknowledgement")
            assert(acceptedNmis == 1 && nmiEntries == 0)
            assert(uint(dut.io.mnPending).testBit(31))
            checkNmi31AfterAck = true
            record("nmi31-ack-with-source-held", s"\"source_high_cycles\":${cycles - start}")
          }
        }
        if (bool(dut.io.icacheFault.valid))
          actualIcacheFaults += ((bool(dut.io.icacheFault.bits.flag), uint(dut.io.icacheFault.bits.value)))
        if (bool(dut.io.prefetchFault.valid)) {
          val ptr = dut.io.prefetchFault.bits.ftqIdx
          actualPrefetchFaults((bool(ptr.flag), uint(ptr.value))) = uint(dut.io.prefetchFault.bits.kind).toInt
        }
        if (bool(dut.io.mEntry)) machineEntries += 1
        if (bool(dut.io.hsEntry)) hostEntries += 1
        if (bool(dut.io.vsEntry)) guestEntries += 1
        if (bool(dut.io.mnEntry)) nmiEntries += 1
        if (bool(dut.io.debugEntry)) debugEntries += 1
        if (bool(dut.io.accepted.valid)) {
          val event = dut.io.accepted.bits
          record("priority-accepted", s"\"identity\":\"${identity(event)}\"")
          if (bool(event.interrupt.nmi) && !bool(event.interrupt.debug) && !bool(event.interrupt.irToHU))
            acceptedNmis += 1
        }

        dut.io.robEnq.foreach { port =>
          if (bool(port.valid)) {
            val fetch = FetchIdentity(bool(port.bits.ftqIdx.flag), uint(port.bits.ftqIdx.value),
              uint(port.bits.ftqOffset).toInt)
            if (instructionPCs.get(fetch).exists(pc => targetPC.contains(pc))) {
              val key = robIdentity(port.bits.robIdx)
              if (targetRob.isEmpty) targetRob = Some(key)
              assert(targetRob.contains(key), "One original instruction must retain one ROB identity")
              val first = bool(port.bits.first)
              val last = bool(port.bits.last)
              if (first) { assert(firstCycle.isEmpty); firstCycle = Some(cycles) }
              if (last) { assert(lastCycle.isEmpty); lastCycle = Some(cycles) }
              enqueued += ((cycles, uint(port.bits.uopIdx).toInt, first, last))
              if (vectorMode) port.bits.numWB.expect(8.U)
              record("boundary-enqueue", s"\"uop\":${uint(port.bits.uopIdx)},\"first\":$first,\"last\":$last")
            }
          }
        }

        if (bool(dut.io.robHead.valid) && targetRob.contains(robIdentity(dut.io.robHead.robIdx))) {
          if (!bool(dut.io.robHead.sealedGroup) && bool(dut.io.timerPending) &&
            bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU))
            unsealedPendingSeen = true
          if (!vectorMode && bool(dut.io.robHead.interruptSafe) && bool(dut.io.robHead.needFlush))
            observedIllegalSafeHead = true
        }
        dut.io.robRetire.foreach { port =>
          if (bool(port.valid) && targetRob.contains(robIdentity(port.bits))) actualRetires += 1
        }

        dut.io.vectorIssue.foreach { port =>
          if (bool(port.valid) && vectorMode && targetRob.contains(robIdentity(port.bits.robIdx))) {
            val index = uint(port.bits.vuopIdx).toInt
            assert(issued.add(index), "The vector instruction issued a duplicate uop")
            port.bits.vl.expect(128.U)
            port.bits.vstart.expect(0.U)
            port.bits.vsew.expect(0.U)
            port.bits.vlmul.expect(3.U)
            record("vector-issue", s"\"uop\":$index,\"vl\":128")
          }
        }
        dut.io.vectorWB.foreach { port =>
          if (bool(port.valid) && vectorMode && targetRob.contains(robIdentity(port.bits.robIdx))) {
            val index = uint(port.bits.vuopIdx).toInt
            assert(writtenBack.add(index), "The vector instruction wrote back a duplicate uop")
            val expected = (0 until 16).foldLeft(BigInt(0))((data, i) => data | (BigInt(expectedByte) << (8 * i)))
            port.bits.data.expect(expected.U)
            port.bits.vxsat.expect(expectedSaturation.B)
            lastWbCycle = cycles
            record("vector-writeback", s"\"uop\":$index,\"saturated\":$expectedSaturation")
          }
        }

        if (bool(dut.io.huReserve.valid) && bool(dut.io.huReserveReady)) {
          if (vectorMode) {
            assert(targetRob.contains(robIdentity(dut.io.huReserve.bits.robIdx)))
            assert(acceptCycle.isEmpty)
            acceptCycle = Some(cycles)
            dut.io.robHead.interruptSafe.expect(true.B)
            dut.io.robHead.sealedGroup.expect(true.B)
            dut.io.robHead.writebacked.expect(true.B)
            dut.io.robHead.needFlush.expect(false.B)
            assert(enqueued.size == 8 && enqueued.map(_._2).distinct.size == 8)
            assert(issued == (0 until 8).toSet && writtenBack == (0 until 8).toSet)
            assert(firstCycle.nonEmpty && lastCycle.nonEmpty && firstCycle.get < lastCycle.get)
            assert(unsealedPendingSeen && cycles > lastWbCycle)
            assert(actualRetires == 0)
          }
          if (holdWithoutNmi) {
            dut.io.traceStall.poke(true.B)
            assert(bool(dut.io.huReserve.valid) && bool(dut.io.huReserveReady))
            holdWithoutNmi = false
            record("watchdog-stall-on-reserve")
          }
          nmiAtReserve.foreach { cause =>
            dut.io.traceStall.poke(true.B)
            dut.io.interrupts.nmi.nmi_31.poke((cause == 31).B)
            dut.io.interrupts.nmi.nmi_43.poke((cause == 43).B)
            assert(bool(dut.io.huReserve.valid) && bool(dut.io.huReserveReady))
            clearNmiAfterEdge = true
            nmiAtReserve = None
            record("one-cycle-nmi-input", s"\"cause\":$cause")
          }
        }
      }

      def afterEdge(): Unit = {
        if (clearNmiAfterEdge) {
          dut.io.interrupts.nmi.nmi_31.poke(false.B)
          dut.io.interrupts.nmi.nmi_43.poke(false.B)
          clearNmiAfterEdge = false
        }
        if (checkNmi31AfterAck) {
          assert(uint(dut.io.mnPending).testBit(31),
            "An asserted external source must win over the actual MN entry acknowledgement")
          nmi31RetainedAtAck = true
          record("nmi31-pending-retained-after-ack")
          dut.io.interrupts.nmi.nmi_31.poke(false.B)
          record("nmi31-source-dropped-after-ack")
          nmi31HoldStart = None
          checkNmi31AfterAck = false
        }
      }

      def finalizeVector(): Unit = {
        assert(vectorMode && acceptCycle.nonEmpty)
        assert(enqueued.size == 8 && issued.size == 8 && writtenBack.size == 8)
        assert(actualRetires == 0)
        assert(firstCycle.get < lastCycle.get && acceptCycle.get > lastWbCycle)
        assert(unsealedPendingSeen)
      }
    }



    // Only transport fields are narrowed; architectural PC and target oracles stay full width.
    def transportBits(value: BigInt, field: UInt): BigInt =
      value & ((BigInt(1) << field.getWidth) - 1)

    // Integer instructions construct full-width operands without poking architectural registers.
    def loadX1Full(value: BigInt): Unit = {
      require(value >= 0 && value < (BigInt(1) << 64))
      val bytes = (0 until 8).map(i => ((value >> (8 * (7 - i))) & 0xff).toInt)
      val significant = bytes.dropWhile(_ == 0)
      if (significant.isEmpty) execute(addi(1, 0, 0))
      else {
        execute(addi(1, 0, significant.head))
        significant.tail.foreach { byte =>
          execute(slli(1, 1, 8))
          if (byte != 0) execute(addi(1, 1, byte))
        }
      }
    }
    def writeFull(address: Int, value: BigInt): Unit = {
      loadX1Full(value)
      val before = acceptedCSRAddresses.size
      if (address == 0x800) requestedTimerWrites.enqueue(value)
      execute((BigInt(address) << 20) | (BigInt(1) << 15) | (BigInt(1) << 12) | 0x73)
      assert(acceptedCSRAddresses.drop(before).toSeq == Seq(address))
      if (address == 0x800) assert(requestedTimerWrites.isEmpty)
    }
    def enterUserAt(satpMode: Int, firstPC: BigInt, userTarget: BigInt): Unit = {
      require(Set(0, 8, 9).contains(satpMode))
      write(0x30c, 1)
      write(0x10c, 1)
      writeFull(0x005, userTarget)
      write(0x000, 1)
      write(0x004, 0x10)
      writeFull(0x180, BigInt(satpMode) << 60)
      write(0x300, 0)
      writeFull(0x341, firstPC)
      execute(BigInt("30200073", 16))
      dut.io.mode.expect(0.U)
      dut.io.virtualMode.expect(false.B)
      ordinaryPC = firstPC
    }


    def runHighHalfPC(satpMode: Int, monitor: PriorityMonitor): Unit = {
      require(satpMode == 8 || satpMode == 9)
      val prefix = if (satpMode == 8) BigInt("ffffffc000000000", 16) else BigInt("ffff800000000000", 16)
      fullAddressOracle = true
      val firstPC = prefix | 0x1200
      val target = prefix | 0x2000
      enterUserAt(satpMode, firstPC, target)
      write(0x800, 1)
      until("HU selected at high canonical address") {
        bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU)
      }
      val pc = firstPC + 0x36
      val source = enqueue(0x13, pc, compressed = true)
      until("full-width target reached real FTQ") {
        entries == 1 && releases == 1 &&
          frontendRedirects.exists(r => r.identity == source && r.fullTarget == target)
      }
      dut.io.uepc.expect(pc.U)
      val event = frontendRedirects.find(r => r.identity == source && r.fullTarget == target).get
      assert(event.target == transportBits(target, dut.io.frontendRedirect.bits.cfiUpdate.target))
      assert(!event.iaf && !event.ipf && !event.igpf)
      assert(reservations == 1 && requests == 1 && completions == 1 && consumes == 1)
      assert(monitor.machineEntries == 0 && monitor.hostEntries == 0 && monitor.guestEntries == 0)
    }





    def verifyOldVectorDestination(): Unit = {
      (24 until 32).foreach { register =>
        execute(vmvXs(3, register))
        execute(x3ToUserScratch)
        dut.io.userScratch.expect(7.U)
      }
      execute(csrReadIntoX3(0xc20))
      execute(x3ToUserScratch)
      dut.io.userScratch.expect(128.U)
      dut.io.fpFlags.expect(0x15.U)
      dut.io.vecStart.expect(0.U)
    }

    def runRvvBoundary(saturating: Boolean, monitor: PriorityMonitor): Unit = {
      enterUser(0x2200)
      execute(vsetFullE8M8)
      execute(vmvVi(8, 1))
      execute(vmvVi(16, if (saturating) -1 else 2))
      execute(vmvVi(24, 7))
      write(0x001, 0x15)
      write(0x009, if (saturating) 0 else 1)
      execute(csrReadIntoX3(0xc20))
      execute(x3ToUserScratch)
      dut.io.userScratch.expect(128.U)
      val pc = BigInt(0x12f6)
      write(0x800, 1)
      until("real pending and HU selection") {
        bool(dut.io.timerPending) && bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU)
      }
      monitor.armVector(pc, saturating, if (saturating) 0xff else 3)
      val target = enqueue(vectorVV(saturating), pc)
      until("whole vector boundary HU entry and real FTQ target") {
        entries == 1 && releases == 1 &&
          frontendRedirects.exists(r => r.identity == target && r.target == 0x2000)
      }
      monitor.finalizeVector()
      assert(!committed(target))
      assert(reservations == 1 && requests == 1 && completions == 1 && consumes == 1)
      dut.io.uepc.expect(pc.U)
      dut.io.vecSat.expect((!saturating).B)
      dut.io.fpFlags.expect(0x15.U)
      dut.io.vecStart.expect(0.U)
      ordinaryPC = 0x2000
      verifyOldVectorDestination()
      dut.io.vecSat.expect((!saturating).B)
    }

    def runIllegalHeadPriority(monitor: PriorityMonitor): Unit = {
      write(0x305, 0x3000)
      enterUser()
      write(0x800, 1)
      until("HU pending before illegal instruction") {
        bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU)
      }
      val pc = BigInt(0x12b6)
      monitor.armIllegal(pc)
      val target = enqueue(checkedInstruction(BigInt(0x53), Instructions.FADD_S), pc)
      until("real illegal instruction trap to M") {
        monitor.machineEntries == 1 && uint(dut.io.mode) == 3 &&
          frontendRedirects.exists(r => r.identity == target && r.target == 0x3000)
      }
      assert(monitor.observedIllegalSafeHead, "This case must exercise a safe head with a real synchronous exception")
      dut.io.mcause.expect(2.U)
      dut.io.mepc.expect(pc.U)
      assert(synchronousTraps.exists(_._2 == pc))
      assert(reservations == 0 && entries == 0 && consumes == 0)
      dut.io.timerPending.expect(true.B)
    }

    def runMachineHostUserOrder(monitor: PriorityMonitor): Unit = {
      write(0x305, 0x3000)
      write(0x105, 0x4000)
      write(0x303, BigInt(1) << 9)
      write(0x304, (BigInt(1) << 9) | (BigInt(1) << 3))
      enterUser()
      write(0x800, 1)
      until("HU pending before higher inputs") {
        bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU)
      }
      dut.io.interrupts.msip.poke(true.B)
      dut.io.interrupts.seip.poke(true.B)
      until("machine software candidate wins") {
        bool(dut.io.candidate.valid) && !bool(dut.io.candidate.bits.irToHU) &&
          !bool(dut.io.candidate.bits.irToHS) && uint(dut.io.candidate.bits.cause) == 3
      }
      val machinePC = BigInt(0x1286)
      val first = enqueue(0x13, machinePC, compressed = true)
      until("machine target consumed") {
        monitor.machineEntries == 1 && frontendRedirects.exists(r => r.identity == first && r.target == 0x3000)
      }
      dut.io.mepc.expect(machinePC.U)
      dut.io.mcause.expect(((BigInt(1) << 63) | 3).U)
      assert(entries == 0 && consumes == 0)
      dut.io.interrupts.msip.poke(false.B)
      ordinaryPC = 0x3000
      execute(BigInt("30200073", 16))
      until("host external candidate wins") {
        bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHS) &&
          uint(dut.io.candidate.bits.cause) == 9
      }
      val hostPC = machinePC
      val second = enqueue(0x13, hostPC, compressed = true)
      until("host target consumed") {
        monitor.hostEntries == 1 && frontendRedirects.exists(r => r.identity == second && r.target == 0x4000)
      }
      dut.io.sepc.expect(hostPC.U)
      dut.io.scause.expect(((BigInt(1) << 63) | 9).U)
      assert(entries == 0 && consumes == 0)
      dut.io.interrupts.seip.poke(false.B)
      ordinaryPC = 0x4000
      execute(BigInt("10200073", 16))
      until("fresh user candidate") { bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU) }
      val userPC = hostPC
      val third = enqueue(0x13, userPC, compressed = true)
      until("user target consumed") {
        entries == 1 && releases == 1 && frontendRedirects.exists(r => r.identity == third && r.target == 0x2000)
      }
      dut.io.uepc.expect(userPC.U)
      assert(monitor.machineEntries == 1 && monitor.hostEntries == 1 && monitor.guestEntries == 0)
      assert(consumes == 1 && reservations == 1 && requests == 1 && completions == 1)
    }

    def runNmiPulseWhileHeld(cause: Int, monitor: PriorityMonitor): Unit = {
      require(cause == 31 || cause == 43)
      write(0x305, 0x3000)
      enterUser()
      dut.io.traceEnable.poke(true.B)
      write(0x800, 1)
      until("HU candidate") { bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU) }
      monitor.holdAndPulseNmiOnNextReserve(cause)
      val userPC = BigInt(0x12d6)
      val user = enqueue(0x13, userPC, compressed = true)
      until("real PC reached reserved CSR") { requests == 1 }
      idle(16)
      assert(entries == 0 && consumes == 0 && completions == 0)
      assert(monitor.acceptedNmis == 0 && monitor.nmiEntries == 0)
      assert(uint(dut.io.mnPending).testBit(cause))
      dut.io.interrupts.nmi.nmi_31.expect(false.B)
      dut.io.interrupts.nmi.nmi_43.expect(false.B)
      dut.io.traceStall.poke(false.B)
      until("original HU target consumed") {
        entries == 1 && releases == 1 && frontendRedirects.exists(r => r.identity == user && r.target == 0x2000)
      }
      dut.io.uepc.expect(userPC.U)
      assert(uint(dut.io.mnPending).testBit(cause))
      if (cause == 31) monitor.holdNmi31AcrossEntry()
      val handlerPC = BigInt(0x2006)
      val handler = enqueue(0x13, handlerPC, compressed = true)
      until("NMI uses a fresh handler instruction boundary") {
        monitor.nmiEntries == 1 && frontendRedirects.exists(r => r.identity == handler && r.target == 0x3000)
      }
      assert(monitor.acceptedNmis == 1)
      dut.io.mnepc.expect(handlerPC.U)
      dut.io.mncause.expect(((BigInt(1) << 63) | cause).U)
      assert((uint(dut.io.mnstatus) & 8) == 0)
      dut.io.handler.expect(true.B)
      if (cause == 31) {
        assert(monitor.nmi31RetainedAtAck && uint(dut.io.mnPending).testBit(31))
        dut.io.interrupts.nmi.nmi_31.expect(false.B)
      } else {
        assert(!uint(dut.io.mnPending).testBit(cause))
      }
      idle(64)
      assert(monitor.acceptedNmis == 1 && monitor.nmiEntries == 1 && entries == 1 && consumes == 1)
      ordinaryPC = 0x3000
      execute(BigInt("70200073", 16))
      dut.io.mode.expect(0.U)
      dut.io.handler.expect(true.B)
      if (cause == 31) {
        assert(uint(dut.io.mnPending).testBit(31))
        until("retained NMI selectable after actual MNRET") {
          bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.nmi) &&
            uint(dut.io.candidate.bits.cause) == 31
        }
        val second = enqueue(0x13, handlerPC, compressed = true)
        assert(second != handler, "The retained NMI must use a fresh handler FTQ transaction")
        until("retained source produces exactly one second MN entry") {
          monitor.nmiEntries == 2 && frontendRedirects.exists(r => r.identity == second && r.target == 0x3000)
        }
        assert(monitor.acceptedNmis == 2)
        dut.io.mnepc.expect(handlerPC.U)
        dut.io.mncause.expect(((BigInt(1) << 63) | 31).U)
        assert((uint(dut.io.mnstatus) & 8) == 0)
        assert(!uint(dut.io.mnPending).testBit(31))
        assert(!committed(handler) && !committed(second))
        idle(64)
        assert(monitor.acceptedNmis == 2 && monitor.nmiEntries == 2 && entries == 1 && consumes == 1)
        ordinaryPC = 0x3000
        execute(BigInt("70200073", 16))
        dut.io.mode.expect(0.U)
        dut.io.handler.expect(true.B)
        ordinaryPC = handlerPC
        execute(0x13)
        idle(64)
        dut.io.interrupts.nmi.nmi_31.expect(false.B)
        assert(!uint(dut.io.mnPending).testBit(31))
        assert(monitor.acceptedNmis == 2 && monitor.nmiEntries == 2 && entries == 1 && consumes == 1)
        record("nmi31-source-ack-retrigger-complete", "\"accepted_nmis\":2,\"mn_entries\":2")
      } else {
        ordinaryPC = handlerPC
        execute(0x13)
        assert(monitor.acceptedNmis == 1 && monitor.nmiEntries == 1)
      }
    }

    def runGuestVsAfterHostCandidate(monitor: PriorityMonitor): Unit = {
      write(0x305, 0x3000)
      enterUser()
      write(0x800, 1)
      until("host candidate before genuine ECALL") {
        bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU)
      }
      val hostPC = BigInt(0x12e6)
      val call = enqueue(BigInt(0x73), hostPC)
      until("host ECALL transferred control to M") {
        monitor.machineEntries == 1 && uint(dut.io.mode) == 3 &&
          frontendRedirects.exists(r => r.identity == call && r.target == 0x3000)
      }
      dut.io.mcause.expect(8.U)
      assert(entries == 0 && consumes == 0)
      ordinaryPC = 0x3000
      write(0x603, 4)
      write(0x304, 4)
      write(0x200, 2)
      write(0x204, 2)
      write(0x205, 0x5000)
      write(0x645, 4)
      write(0x300, 0)
      setCSRBit(0x300, 39)
      write(0x341, 0x1800)
      execute(BigInt("30200073", 16))
      dut.io.mode.expect(0.U)
      dut.io.virtualMode.expect(true.B)
      until("delegated virtual supervisor candidate") {
        bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToVS) &&
          uint(dut.io.candidate.bits.cause) == 2
      }
      assert(!bool(dut.io.candidate.bits.irToHU))
      dut.io.timerPending.expect(true.B)
      dut.io.timerTick.expect(false.B)
      val guestPC = BigInt(0x1816)
      val guest = enqueue(0x13, guestPC, compressed = true)
      until("VS software interrupt target consumed") {
        monitor.guestEntries == 1 && uint(dut.io.mode) == 1 && bool(dut.io.virtualMode) &&
          frontendRedirects.exists(r => r.identity == guest && r.target == 0x5000)
      }
      dut.io.vsepc.expect(guestPC.U)
      dut.io.vscause.expect(((BigInt(1) << 63) | 1).U)
      assert(entries == 0 && consumes == 0 && reservations == 0)
      dut.io.timerPending.expect(true.B)
    }



    // A real backend target fault supplies the following normal IFU exception packet.
    def runBadUtvec(satpMode: Int, monitor: PriorityMonitor): Unit = {
      require(Set(0, 8, 9).contains(satpMode))
      val target = (BigInt(1) << (if (satpMode == 0) dut.PAddrBits else if (satpMode == 8) 39 else 48)) | 0x2000
      write(0x305, 0x3000)
      enterUserAt(satpMode, 0x1200, target)
      write(0x800, 1)
      until("HU candidate before malformed direct target") {
        bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU)
      }
      val sourcePC = BigInt(0x1236)
      val source = enqueue(0x13, sourcePC, compressed = true)
      until("malformed full target and flags reached real FTQ") {
        entries == 1 && releases == 1 &&
          frontendRedirects.exists(r => r.identity == source && r.fullTarget == target)
      }
      val delivered = frontendRedirects.find(r => r.identity == source && r.fullTarget == target).get
      assert(delivered.iaf == (satpMode == 0) && delivered.ipf == (satpMode != 0) && !delivered.igpf)
      assert(delivered.target == transportBits(target, dut.io.frontendRedirect.bits.cfiUpdate.target))
      dut.io.uepc.expect(sourcePC.U)
      dut.io.utval.expect(0.U)
      dut.io.handler.expect(true.B)
      assert(entries == 1 && consumes == 1 && monitor.machineEntries == 0)

      val fault = enqueue(0x13, target, faultFromFtq = true)
      assert(monitor.actualIcacheFaults((fault.flag, fault.index)))
      assert(monitor.actualPrefetchFaults((fault.flag, fault.index)) == (if (satpMode == 0) 3 else 1))
      until("ordinary target-fetch fault trapped to M") {
        monitor.machineEntries == 1 && uint(dut.io.mode) == 3 &&
          frontendRedirects.exists(r => r.identity == fault && r.target == 0x3000)
      }
      dut.io.mcause.expect((if (satpMode == 0) 1 else 12).U)
      dut.io.mepc.expect(target.U)
      dut.io.mtval.expect(target.U)
      dut.io.uepc.expect(sourcePC.U)
      dut.io.utval.expect(0.U)
      dut.io.handler.expect(true.B)
      assert(entries == 1 && consumes == 1 && reservations == 1 && completions == 1 && releases == 1)
    }




    // Batch only the inactive interval before the unchanged hardware watchdog threshold.
    def batchUntilWatchdogGuard(): Unit = {
      assert(acceptedIdentity.nonEmpty && reservations == 1 && requests == 1 && completions == 0)
      assert(entries == 0 && consumes == 0 && !bool(dut.io.handler) && !bool(dut.io.debugMode))
      assert(bool(dut.io.traceEnable) && bool(dut.io.traceStall))
      assert(!bool(dut.io.instruction.valid) && !bool(dut.io.bpu.resp.valid) && !bool(dut.io.predecode.valid))
      assert(!bool(dut.io.criticalErrorState))
      val before = Seq(uint(dut.io.cumulativeHuEffects), uint(dut.io.cumulativeConsumes),
        uint(dut.io.cumulativeCompletions), uint(dut.io.cumulativeBackendRedirects),
        uint(dut.io.cumulativeDebugEntries))
      val ticksBefore = uint(dut.io.cumulativeQualifiedTicks)
      val watchdogBefore = uint(dut.io.robCommitStuck)
      val remaining = (BigInt(1) << 21) - 1 - watchdogBefore - 64
      assert(remaining > 0)
      dut.clock.step(remaining.toInt)
      val after = Seq(uint(dut.io.cumulativeHuEffects), uint(dut.io.cumulativeConsumes),
        uint(dut.io.cumulativeCompletions), uint(dut.io.cumulativeBackendRedirects),
        uint(dut.io.cumulativeDebugEntries))
      assert(after == before, "An architectural effect or target occurred inside the inactive batch")
      val tickDelta = uint(dut.io.cumulativeQualifiedTicks) - ticksBefore
      assert(tickDelta == remaining)
      qualifiedTicks += tickDelta
      cycles += remaining.toInt
      assert(uint(dut.io.robCommitStuck) == watchdogBefore + remaining)
      record("watchdog-idle-batch", s"\"cycles\":$remaining,\"effects\":0,\"consumes\":0,\"redirects\":0")
      dut.io.timerPending.expect(true.B)
      dut.io.timerRemaining.expect(0.U)
    }




    private var fullAddressOracle = false
    // Narrow signed PC outputs are expanded without changing the full-width source oracle.
    private def architecturalPCResult(value: BigInt, width: Int): BigInt = {
      if (fullAddressOracle && value.testBit(width - 1))
        value | (((BigInt(1) << 64) - 1) ^ ((BigInt(1) << width) - 1))
      else value
    }
    private def startPriorityScenario(name: String): PriorityMonitor = {
      resetMatrixScenario(name)
      val monitor = new PriorityMonitor
      activePriorityMonitor = Some(monitor)
      matrixObservationsEnabled = true
      monitor
    }
    private def finishPriorityScenario(monitor: PriorityMonitor): Unit = {
      idle(16)
      finishMatrixScenario()
      record("priority-scenario-end", s"\"machine\":${monitor.machineEntries},\"host\":${monitor.hostEntries}," +
        s"\"guest\":${monitor.guestEntries},\"nmi\":${monitor.nmiEntries},\"debug\":${monitor.debugEntries}," +
        s"\"accepted_nmi\":${monitor.acceptedNmis}")
    }
    def priorityScenario(name: String): Unit = {
      val monitor = startPriorityScenario(name)
      name match {
        case "rvv-add-whole" => runRvvBoundary(false, monitor)
        case "rvv-sat-unretired" => runRvvBoundary(true, monitor)
        case "safe-fp-exception-over-hu" => runIllegalHeadPriority(monitor)
        case "machine-host-user-order" => runMachineHostUserOrder(monitor)
        case "guest-vs-host-cancel" => runGuestVsAfterHostCandidate(monitor)
        case "nmi31-pulse-held-hu" => runNmiPulseWhileHeld(31, monitor)
        case "nmi43-pulse-held-hu" => runNmiPulseWhileHeld(43, monitor)
        case "high-half-satp-8" => runHighHalfPC(8, monitor)
        case "high-half-satp-9" => runHighHalfPC(9, monitor)
        case "bad-utvec-satp-0" => runBadUtvec(0, monitor)
        case "bad-utvec-satp-8" => runBadUtvec(8, monitor)
        case "bad-utvec-satp-9" => runBadUtvec(9, monitor)
        case other => throw new IllegalArgumentException(s"Unknown priority scenario: $other")
      }
      finishPriorityScenario(monitor)
    }
    def extendedWatchdog(): Unit = {
      val monitor = startPriorityScenario("critical-debug-via-real-watchdog")
      dut.io.interrupts.debug.poke(true.B)
      until("real debug interrupt selected") {
        bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.debug)
      }
      val haltPC = BigInt(0x1106)
      val halt = enqueue(0x13, haltPC, compressed = true)
      until("real haltreq debug entry") {
        monitor.debugEntries == 1 && bool(dut.io.debugMode) &&
          frontendRedirects.exists(r => r.identity == halt && r.target == dut.debugEntryAddress)
      }
      dut.io.dpc.expect(haltPC.U)
      dut.io.interrupts.debug.poke(false.B)
      ordinaryPC = dut.debugEntryAddress
      write(0x30c, 1)
      write(0x10c, 1)
      write(0x005, 0x2000)
      write(0x000, 1)
      write(0x004, 0x10)
      write(0x300, 0)
      write(0x7b0, 0x80000)
      write(0x7b1, 0x1200)
      execute(BigInt("7b200073", 16))
      dut.io.mode.expect(0.U)
      dut.io.debugMode.expect(false.B)
      ordinaryPC = 0x1200
      dut.io.traceEnable.poke(true.B)
      write(0x800, 1)
      until("HU timer candidate before watchdog stall") {
        bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU)
      }
      monitor.holdWithoutNmiOnNextReserve()
      val savedPC = BigInt(0x1236)
      val interrupted = enqueue(0x13, savedPC, compressed = true)
      until("reserved CSR PC with a blocked completion") {
        requests == 1 && bool(dut.io.huCompletion.valid) && !bool(dut.io.huCompletionReady) && bool(dut.io.traceBlocked)
      }
      val bankBefore = Seq(uint(dut.io.ustatus), uint(dut.io.uepc), uint(dut.io.ucause), uint(dut.io.utval))
      batchUntilWatchdogGuard()
      until("real watchdog critical debug effect", maximum = 128) {
        monitor.debugEntries == 2 && bool(dut.io.debugMode) && bool(dut.io.criticalErrorState)
      }
      dut.io.dpc.expect(savedPC.U)
      assert(entries == 0 && consumes == 0 && uint(dut.io.cumulativeHuEffects) == 0 &&
        uint(dut.io.cumulativeConsumes) == 0)
      assert(Seq(uint(dut.io.ustatus), uint(dut.io.uepc), uint(dut.io.ucause), uint(dut.io.utval)) == bankBefore)
      dut.io.timerPending.expect(true.B)
      dut.io.handler.expect(false.B)
      until("critical debug terminal is held") {
        bool(dut.io.huCompletion.valid) && uint(dut.io.huCompletion.bits.outcome) == 2 &&
          bool(dut.io.huEffectLocked) && !bool(dut.io.huCompletionReady)
      }
      dut.io.traceStall.poke(false.B)
      until("critical debug target and terminal release") {
        releases == 1 && frontendRedirects.exists(r => r.identity == interrupted &&
          r.fullTarget == dut.debugEntryAddress)
      }
      assert(reservations == 1 && requests == 1 && completions == 1 && entries == 0 && consumes == 0)
      assert(encodedHU.isEmpty)
      finishPriorityScenario(monitor)
    }
    private var activeWatchdogBoundary: Option[WatchdogBoundaryMonitor] = None

    // Every timing point is sampled from the real watchdog and transaction ports.
    private class WatchdogBoundaryMonitor(val postEffect: Boolean, val sourcePC: BigInt) {
      var sourceRob: Option[RobIdentity] = None
      var overflowCycle: Option[Int] = None
      var robErrorCycle: Option[Int] = None
      var controlErrorCycle: Option[Int] = None
      var csrInputCycle: Option[Int] = None
      var criticalCycle: Option[Int] = None
      var reserveCycle: Option[Int] = None
      var requestCycle: Option[Int] = None
      var effectCycle: Option[Int] = None
      var releaseCycle: Option[Int] = None
      var deferredCycle: Option[Int] = None
      var criticalAcceptCycle: Option[Int] = None
      var criticalAcceptFetch: Option[FetchIdentity] = None
      var debugCycle: Option[Int] = None
      var beforePcObserved = false
      var afterEffectObserved = false
      var cancellationBeforePcObserved = false
      var savedDpc: Option[BigInt] = None
      val freshHandlerEnqueues = mutable.Set.empty[(RobIdentity, FetchIdentity)]

      def beforeEdge(): Unit = {
        dut.io.robEnq.foreach { port =>
          if (bool(port.valid)) {
            val fetch = FetchIdentity(bool(port.bits.ftqIdx.flag), uint(port.bits.ftqIdx.value), uint(port.bits.ftqOffset).toInt)
            if (instructionPCs.get(fetch).contains(sourcePC)) {
              val id = robIdentity(port.bits.robIdx)
              sourceRob.foreach(previous => assert(previous == id))
              sourceRob = Some(id)
            } else if (postEffect && releaseCycle.exists(_ < cycles)) {
              freshHandlerEnqueues += ((robIdentity(port.bits.robIdx), fetch))
            }
          }
        }
        if (bool(dut.io.watchdogOverflow) && overflowCycle.isEmpty) {
          overflowCycle = Some(cycles)
          assert(uint(dut.io.robCommitStuck) == (BigInt(1) << 21) - 1)
          record("watchdog-overflow-source")
        }
        if (bool(dut.io.robWatchdogError) && robErrorCycle.isEmpty) {
          robErrorCycle = Some(cycles)
          assert(overflowCycle.contains(cycles - 2))
          record("watchdog-rob-error")
        }
        if (bool(dut.io.controlWatchdogError) && controlErrorCycle.isEmpty) {
          controlErrorCycle = Some(cycles)
          assert(robErrorCycle.contains(cycles - 2))
          record("watchdog-control-error")
        }
        if (bool(dut.io.csrCriticalInput) && csrInputCycle.isEmpty) {
          csrInputCycle = Some(cycles)
          assert(controlErrorCycle.contains(cycles))
          record("watchdog-csr-input")
        }
        if (bool(dut.io.huReserve.valid) && bool(dut.io.huReserveReady)) {
          assert(reserveCycle.isEmpty && sourceRob.contains(robIdentity(dut.io.huReserve.bits.robIdx)))
          reserveCycle = Some(cycles)
          savedDpc = Some(uint(dut.io.dpc))
          dut.io.criticalErrorState.expect(false.B)
        }
        if (bool(dut.io.huRequest.valid) && bool(dut.io.huRequestReady)) {
          assert(requestCycle.isEmpty && reserveCycle.nonEmpty)
          requestCycle = Some(cycles)
          dut.io.huRequest.bits.pc.expect(sourcePC.U)
        }
        if (bool(dut.io.entryEffect)) {
          assert(postEffect && effectCycle.isEmpty)
          effectCycle = Some(cycles)
        }
        if (bool(dut.io.huRelease)) {
          assert(releaseCycle.isEmpty)
          releaseCycle = Some(cycles)
        }
        if (bool(dut.io.criticalErrorState) && criticalCycle.isEmpty) {
          criticalCycle = Some(cycles)
          assert(csrInputCycle.contains(cycles - 1) && overflowCycle.contains(cycles - 5),
            "The watchdog critical source must traverse the actual two plus two plus one register path")
          assert(reserveCycle.nonEmpty && releases == 0 && acceptedIdentity.nonEmpty)
          if (postEffect) {
            assert(effectCycle.contains(cycles - 1) && entries == 1 && consumes == 1 && completions == 1)
            dut.io.huEffectLocked.expect(true.B)
            dut.io.huRelease.expect(false.B)
            afterEffectObserved = true
            record("critical-after-effect-before-release")
          } else {
            assert(reserveCycle.contains(cycles - 1) && requestCycle.isEmpty && requests == 0)
            dut.io.huRequest.valid.expect(false.B)
            dut.io.entryEffect.expect(false.B)
            dut.io.debugEntry.expect(false.B)
            beforePcObserved = true
            record("critical-before-reserved-pc")
          }
          dut.io.dpc.expect(savedDpc.get.U)
        }
        if (!postEffect && criticalCycle.nonEmpty && requestCycle.isEmpty) {
          dut.io.debugEntry.expect(false.B)
          dut.io.entryEffect.expect(false.B)
          dut.io.timerConsume.expect(false.B)
          if (bool(dut.io.huCanceled)) cancellationBeforePcObserved = true
        }
        if (bool(dut.io.criticalDebugDeferred) && deferredCycle.isEmpty) {
          assert(postEffect && afterEffectObserved && criticalCycle.contains(cycles - 1))
          deferredCycle = Some(cycles)
          record("critical-deferred-after-hu-effect")
        }
        if (bool(dut.io.accepted.valid) && bool(dut.io.accepted.bits.interrupt.criticalDebug)) {
          assert(postEffect && criticalAcceptCycle.isEmpty && deferredCycle.nonEmpty && releases == 1)
          assert(bool(dut.io.accepted.bits.interrupt.debug) && !bool(dut.io.accepted.bits.interrupt.irToHU))
          criticalAcceptCycle = Some(cycles)
          criticalAcceptFetch = Some(fetchIdentity(dut.io.accepted.bits))
          // A flush can reuse its ROB index. A later real enqueue and its distinct
          // FTQ/PC identity establish the new instruction's ownership instead.
          assert(freshHandlerEnqueues((robIdentity(dut.io.accepted.bits.robIdx), criticalAcceptFetch.get)),
            "Deferred critical debug must claim a fresh post-release ROB enqueue")
          assert(!instructionPCs.get(criticalAcceptFetch.get).contains(sourcePC))
          record("deferred-critical-accepted", s"\"identity\":\"${identity(dut.io.accepted.bits)}\"")
        }
        if (bool(dut.io.debugEntry)) {
          assert(debugCycle.isEmpty && criticalCycle.nonEmpty)
          if (postEffect) assert(criticalAcceptCycle.nonEmpty && criticalAcceptCycle.get < cycles)
          else assert(requestCycle.nonEmpty && requestCycle.get < cycles && beforePcObserved)
          debugCycle = Some(cycles)
          record("watchdog-boundary-debug-effect")
        }
        if (postEffect && criticalCycle.nonEmpty && criticalAcceptCycle.isEmpty) {
          dut.io.debugEntry.expect(false.B)
          dut.io.dpc.expect(savedDpc.get.U)
        }
      }

      def readyHead: Boolean = sourceRob.nonEmpty && bool(dut.io.robHead.valid) &&
        sourceRob.contains(robIdentity(dut.io.robHead.robIdx)) && bool(dut.io.robHead.interruptSafe) &&
        bool(dut.io.robHead.sealedGroup) && bool(dut.io.robHead.writebacked) && !bool(dut.io.robHead.needFlush)
    }

    private def prepareWatchdogBoundary(postEffect: Boolean): (PriorityMonitor, WatchdogBoundaryMonitor, FetchIdentity) = {
      val monitor = startPriorityScenario(if (postEffect) "critical-after-hu-effect" else "critical-before-hu-pc")
      dut.io.interrupts.debug.poke(true.B)
      until("real debug interrupt selected") { bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.debug) }
      val haltPC = BigInt(0x1106)
      val halt = enqueue(0x13, haltPC, compressed = true)
      until("real haltreq debug entry") {
        monitor.debugEntries == 1 && bool(dut.io.debugMode) &&
          frontendRedirects.exists(r => r.identity == halt && r.target == dut.debugEntryAddress)
      }
      dut.io.interrupts.debug.poke(false.B)
      ordinaryPC = dut.debugEntryAddress
      write(0x30c, 1)
      write(0x10c, 1)
      write(0x005, 0x2000)
      write(0x000, 1)
      write(0x004, 0x10)
      write(0x300, 0)
      write(0x7b0, 0x80000)
      write(0x7b1, 0x1200)
      execute(BigInt("7b200073", 16))
      dut.io.mode.expect(0.U)
      dut.io.debugMode.expect(false.B)
      ordinaryPC = 0x1200
      dut.io.traceEnable.poke(true.B)
      write(0x800, 1)
      until("pending HU candidate before preparing the watchdog head") {
        bool(dut.io.timerPending) && bool(dut.io.robCandidate.valid) && bool(dut.io.robCandidate.bits.irToHU)
      }
      dut.io.traceStall.poke(true.B)
      until("real trace backpressure") { bool(dut.io.traceBlocked) }
      val savedPC = BigInt(0x1236)
      val boundary = new WatchdogBoundaryMonitor(postEffect, savedPC)
      activeWatchdogBoundary = Some(boundary)
      val source = enqueue(0x13, savedPC, compressed = true)
      until("actual complete safe head held before HU acceptance") { boundary.readyHead }
      idle(8)
      assert(!committed(source) && reservations == 0 && requests == 0 && entries == 0 && consumes == 0)
      (monitor, boundary, source)
    }

    private def batchPreparedHeadToWatchdogGuard(boundary: WatchdogBoundaryMonitor): Unit = {
      assert(boundary.readyHead && acceptedIdentity.isEmpty && reservations == 0 && requests == 0 && completions == 0)
      assert(entries == 0 && consumes == 0 && !bool(dut.io.handler) && !bool(dut.io.debugMode))
      assert(bool(dut.io.traceEnable) && bool(dut.io.traceStall) && bool(dut.io.traceBlocked))
      assert(!bool(dut.io.instruction.valid) && !bool(dut.io.bpu.resp.valid) && !bool(dut.io.predecode.valid))
      assert(!bool(dut.io.criticalErrorState) && !bool(dut.io.watchdogOverflow))
      def evidence: Seq[BigInt] = Seq(uint(dut.io.cumulativeHuEffects), uint(dut.io.cumulativeConsumes),
        uint(dut.io.cumulativeCompletions), uint(dut.io.cumulativeBackendRedirects), uint(dut.io.cumulativeDebugEntries),
        uint(dut.io.cumulativeHuReservations), uint(dut.io.cumulativeHuRequests), uint(dut.io.cumulativeHuReleases),
        uint(dut.io.cumulativeRobRetireCycles))
      val before = evidence
      val ticksBefore = uint(dut.io.cumulativeQualifiedTicks)
      val watchdogBefore = uint(dut.io.robCommitStuck)
      val clocks = (BigInt(1) << 21) - 1 - watchdogBefore - 64
      assert(clocks > 0)
      dut.clock.step(clocks.toInt)
      assert(evidence == before, "The inactive watchdog batch changed architectural or transaction state")
      val tickDelta = uint(dut.io.cumulativeQualifiedTicks) - ticksBefore
      assert(tickDelta == clocks)
      qualifiedTicks += tickDelta
      cycles += clocks.toInt
      assert(uint(dut.io.robCommitStuck) == watchdogBefore + clocks && boundary.readyHead)
      dut.io.timerPending.expect(true.B)
      dut.io.timerRemaining.expect(0.U)
      dut.io.criticalErrorState.expect(false.B)
      record("watchdog-preacceptance-batch", s"\"clocks\":$clocks,\"reserves\":0,\"requests\":0," +
        "\"effects\":0,\"consumes\":0,\"redirects\":0,\"retire_cycles\":0")
    }

    def watchdogBeforeReservedPC(): Unit = {
      val (monitor, boundary, source) = prepareWatchdogBoundary(postEffect = false)
      val bankBefore = Seq(uint(dut.io.ustatus), uint(dut.io.uepc), uint(dut.io.ucause), uint(dut.io.utval))
      batchPreparedHeadToWatchdogGuard(boundary)
      until("unchanged real watchdog overflow", maximum = 128) { bool(dut.io.watchdogOverflow) }
      val overflow = cycles
      // T+3 release makes the registered trace gate open at A0=T+4. The CSR
      // critical latch rises at T+5 while the original PC still crosses ExeUnit.
      idle(3)
      assert(boundary.overflowCycle.contains(overflow) && cycles == overflow + 3)
      dut.io.csrCriticalInput.expect(false.B)
      dut.io.criticalErrorState.expect(false.B)
      dut.io.traceStall.poke(false.B)
      record("watchdog-release-before-reserved-pc", s"\"overflow_cycle\":$overflow")
      until("critical debug after the still-required PC handoff", maximum = 128) {
        monitor.debugEntries == 2 && releases == 1 && bool(dut.io.debugMode) &&
          frontendRedirects.exists(r => r.identity == source && r.fullTarget == dut.debugEntryAddress)
      }
      assert(boundary.reserveCycle.contains(overflow + 4))
      assert(boundary.beforePcObserved && boundary.cancellationBeforePcObserved)
      assert(boundary.requestCycle.contains(boundary.reserveCycle.get + 3))
      assert(boundary.debugCycle.nonEmpty && boundary.debugCycle.get > boundary.requestCycle.get)
      dut.io.dpc.expect(boundary.sourcePC.U)
      assert(Seq(uint(dut.io.ustatus), uint(dut.io.uepc), uint(dut.io.ucause), uint(dut.io.utval)) == bankBefore)
      dut.io.handler.expect(false.B)
      dut.io.timerPending.expect(true.B)
      assert(entries == 0 && consumes == 0 && reservations == 1 && requests == 1 && completions == 1 && releases == 1)
      assert(encodedHU.isEmpty && !committed(source))
      assert(events.exists(line => line.contains("\"event\":\"csr-completion\"") && line.contains("\"outcome\":2")))
      finishPriorityScenario(monitor)
      activeWatchdogBoundary = None
    }

    def watchdogAfterEntryBeforeRelease(): Unit = {
      val (monitor, boundary, source) = prepareWatchdogBoundary(postEffect = true)
      batchPreparedHeadToWatchdogGuard(boundary)
      until("one cycle before unchanged watchdog overflow", maximum = 128) {
        uint(dut.io.robCommitStuck) == (BigInt(1) << 21) - 2
      }
      val overflow = cycles + 1
      // T-1 release gives A0=T and E=T+4. Critical arrives E+1 while the
      // immutable HU target still owns the following E+2 frontend delivery.
      dut.io.traceStall.poke(false.B)
      record("watchdog-release-before-hu-effect", s"\"overflow_cycle\":$overflow")
      until("original HU target survived post-effect critical arrival", maximum = 128) {
        entries == 1 && releases == 1 && boundary.deferredCycle.nonEmpty &&
          frontendRedirects.exists(r => r.identity == source && r.fullTarget == 0x2000)
      }
      assert(boundary.reserveCycle.contains(overflow) && boundary.effectCycle.contains(overflow + 4))
      assert(boundary.afterEffectObserved && boundary.releaseCycle.contains(overflow + 6))
      assert(boundary.criticalCycle.contains(overflow + 5) && boundary.deferredCycle.contains(overflow + 6))
      dut.io.uepc.expect(boundary.sourcePC.U)
      dut.io.ucause.expect(((BigInt(1) << 63) | 4).U)
      dut.io.utval.expect(0.U)
      dut.io.ustatus.expect(0x10.U)
      dut.io.handler.expect(true.B)
      dut.io.timerPending.expect(false.B)
      dut.io.debugMode.expect(false.B)
      assert(monitor.debugEntries == 1 && entries == 1 && consumes == 1 && completions == 1)
      until("deferred critical descriptor at the real ROB input", maximum = 128) {
        bool(dut.io.robCandidate.valid) && bool(dut.io.robCandidate.bits.criticalDebug)
      }
      val handlerPC = BigInt(0x2006)
      val handler = enqueue(0x13, handlerPC, compressed = true)
      // The fresh debug event uses the legacy redirect target; fullTarget is a trap-value sideband.
      until("deferred critical debug at the fresh handler boundary", maximum = 128) {
        monitor.debugEntries == 2 && bool(dut.io.debugMode) &&
          frontendRedirects.exists(r => r.identity == handler && r.target == dut.debugEntryAddress)
      }
      val debugRedirects = frontendRedirects.filter(_.identity == handler)
      assert(debugRedirects.size == 1 && debugRedirects.head.target == dut.debugEntryAddress &&
        !debugRedirects.head.iaf && !debugRedirects.head.ipf && !debugRedirects.head.igpf)
      assert(boundary.criticalAcceptFetch.contains(handler) && source != handler)
      dut.io.dpc.expect(handlerPC.U)
      dut.io.uepc.expect(boundary.sourcePC.U)
      dut.io.handler.expect(true.B)
      dut.io.timerPending.expect(false.B)
      dut.io.criticalDebugDeferred.expect(false.B)
      assert(!committed(source) && !committed(handler))
      assert(reservations == 1 && requests == 1 && completions == 1 && releases == 1 && entries == 1 && consumes == 1)
      assert(encodedHU.count(_._2 == source) == 1)
      finishPriorityScenario(monitor)
      activeWatchdogBoundary = None
    }

    def disabledBackendSmoke(): Unit = {
      require(!enabled)
      val monitor = startPriorityScenario("disabled-production-backend-smoke")
      execute(0x13)
      write(0x305, 0x3000)
      val csrPC = BigInt(0x1100)
      val csr = enqueue(csrReadIntoX3(0x000), csrPC)
      until("disabled user CSR is illegal") {
        monitor.machineEntries == 1 && frontendRedirects.exists(r => r.identity == csr && r.target == 0x3000)
      }
      dut.io.mcause.expect(2.U)
      ordinaryPC = 0x3000
      write(0x300, 0)
      val ret = enqueue(BigInt("00200073", 16), ordinaryPC)
      until("disabled URET is illegal") {
        monitor.machineEntries == 2 && frontendRedirects.exists(r => r.identity == ret && r.target == 0x3000)
      }
      dut.io.mcause.expect(2.U)
      assert(entries == 0 && consumes == 0 && reservations == 0 && requests == 0 && completions == 0)
      dut.io.candidate.valid.expect(false.B)
      dut.io.handler.expect(false.B)
      finishPriorityScenario(monitor)
    }


    private case class ScalarRenameRecord(original: FetchIdentity, rob: (Boolean, BigInt),
      offset: BigInt, size: Int, first: Boolean, last: Boolean, canCompress: Boolean, needsRob: Boolean, fused: Boolean)
    private var scalarMembers = Seq.empty[FetchIdentity]
    private var scalarFusionExpected = false
    private var scalarActive = false
    private var scalarAccepted = false
    private var scalarGroup: Option[(Boolean, BigInt)] = None
    private var scalarRenameRecords = mutable.ArrayBuffer.empty[ScalarRenameRecord]
    private var scalarRobRecords = mutable.ArrayBuffer.empty[((Boolean, BigInt), BigInt, Boolean, Boolean, Int, Int, Boolean)]
    private var scalarFusionCount = 0
    private var scalarWbCount = 0
    private var scalarLastWb = -1
    private var scalarRetires = 0

    // These are normal frontend lanes; a partially accepted prefix advances just like the IBuffer.
    private def enqueueScalarPacket(members: Seq[(BigInt, Int)], firstPC: BigInt): Seq[FetchIdentity] = {
      val lanes = Seq(dut.io.instruction) ++ dut.io.instructionTail.toSeq
      require(members.nonEmpty && members.size <= lanes.size && members.forall(x => x._2 == 2 || x._2 == 4))
      val byteWidth = dut.io.predecode.bits.pd.length * 2
      val base = firstPC - firstPC % byteWidth
      val pcs = members.scanLeft(firstPC) { case (pc, (_, length)) => pc + length }.dropRight(1)
      require(pcs.last + members.last._2 <= base + byteWidth)
      val flag = bool(dut.io.ftqNext.flag)
      val index = uint(dut.io.ftqNext.value)
      val ids = pcs.map(pc => FetchIdentity(flag, index, ((pc - base) / 2).toInt))
      ids.zip(pcs).foreach { case (id, pc) => instructionPCs(id) = pc; committed -= id }
      scalarMembers = ids
      fetches -= ((flag, index))
      zero(dut.io.bpu.resp.bits)
      prediction(dut.io.bpu.resp.bits.s1, base, flag, index)
      dut.io.bpu.resp.valid.poke(true.B)
      until("same-FTQ prediction acceptance")(bool(dut.io.bpu.resp.ready))
      edge()
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits.s1)
      prediction(dut.io.bpu.resp.bits.s2, base, flag, index)
      edge()
      zero(dut.io.bpu.resp.bits.s2)
      prediction(dut.io.bpu.resp.bits.s3, base, flag, index)
      edge()
      zero(dut.io.bpu.resp.bits.s3)
      until("same-FTQ fetch request")(fetches((flag, index)))
      zero(dut.io.predecode.bits)
      setPointer(dut.io.predecode.bits.ftqIdx, flag, index)
      dut.io.predecode.bits.pc.zipWithIndex.foreach { case (field, i) =>
        field.poke(transportBits(base + 2 * i, field).U)
      }
      ids.zip(members).foreach { case (id, (_, length)) =>
        dut.io.predecode.bits.pd(id.offset).valid.poke(true.B)
        dut.io.predecode.bits.pd(id.offset).isRVC.poke((length == 2).B)
        dut.io.predecode.bits.instrRange(id.offset).poke(true.B)
      }
      dut.io.predecode.bits.ftqOffset.poke(ids.last.offset.U)
      dut.io.predecode.bits.target.poke(transportBits(pcs.last + members.last._2, dut.io.predecode.bits.target).U)
      dut.io.predecode.valid.poke(true.B)
      edge()
      dut.io.predecode.valid.poke(false.B)
      var consumed = 0
      var waited = 0
      while (consumed < members.size && waited < 2000) {
        lanes.zipWithIndex.foreach { case (lane, slot) =>
          val original = consumed + slot
          lane.valid.poke((original < members.size).B)
          zero(lane.bits)
          if (original < members.size) {
            val (instruction, length) = members(original)
            val id = ids(original)
            lane.bits.instr.poke(instruction.U)
            lane.bits.pc.poke(transportBits(pcs(original), lane.bits.pc).U)
            lane.bits.trigger.poke(xiangshan.TriggerAction.None)
            lane.bits.pd.valid.poke(true.B)
            lane.bits.pd.isRVC.poke((length == 2).B)
            setPointer(lane.bits.ftqPtr, id.flag, id.index)
            lane.bits.ftqOffset.poke(id.offset.U)
            lane.bits.isLastInFtqEntry.poke((original == members.size - 1).B)
          }
        }
        val available = members.size - consumed
        val accepted = (0 until available).takeWhile(i => bool(lanes(i).ready)).size
        assert((0 until available).count(i => bool(lanes(i).ready)) == accepted,
          "The normal frontend interface must accept a prefix")
        edge()
        consumed += accepted
        waited += 1
      }
      assert(consumed == members.size, "The real frontend did not accept the same-FTQ packet")
      lanes.foreach { lane => lane.valid.poke(false.B); zero(lane.bits) }
      instructions += members.size
      record("same-ftq-packet", "\"members\":" + members.size + ",\"first_pc\":\"0x" + firstPC.toString(16) + "\"")
      ids
    }

    private def beforeScalarEdge(): Unit = {
      if (!scalarActive || scalarMembers.isEmpty) return
      val packet = scalarMembers.head
      def inPacket(ptr: FtqPtr): Boolean = bool(ptr.flag) == packet.flag && uint(ptr.value) == packet.index
      dut.io.scalarRename.foreach { port =>
        if (bool(port.valid) && inPacket(port.bits.inputFtqIdx)) {
          val original = FetchIdentity(packet.flag, packet.index, uint(port.bits.inputOffset).toInt)
          scalarRenameRecords += ScalarRenameRecord(original, robIdentity(port.bits.robIdx),
            uint(port.bits.outputOffset), uint(port.bits.instrSize).toInt, bool(port.bits.first), bool(port.bits.last),
            bool(port.bits.canCompress), bool(port.bits.needsRob), bool(port.bits.fused))
          record("scalar-rename", "\"input_offset\":" + original.offset + ",\"output_offset\":" +
            uint(port.bits.outputOffset) + ",\"size\":" + uint(port.bits.instrSize) + ",\"fused\":" + bool(port.bits.fused))
        }
      }
      dut.io.scalarFusion.foreach { port =>
        if (bool(port.valid) && inPacket(port.bits.ftqIdx) && uint(port.bits.ftqOffset) == packet.offset) {
          port.bits.clearNext.expect(true.B)
          scalarFusionCount += 1
          record("actual-fusion", "\"offset\":" + packet.offset)
        }
      }
      dut.io.robEnq.foreach { port =>
        if (bool(port.valid) && inPacket(port.bits.ftqIdx)) {
          val key = robIdentity(port.bits.robIdx)
          scalarRobRecords += ((key, uint(port.bits.ftqOffset), bool(port.bits.first), bool(port.bits.last),
            uint(port.bits.numWB).toInt, uint(port.bits.instrSize).toInt, bool(port.bits.fused)))
          if (uint(port.bits.ftqOffset) == packet.offset) {
            scalarGroup.foreach(saved => assert(saved == key))
            scalarGroup = Some(key)
          }
        }
      }
      dut.io.scalarWriteback.foreach { port =>
        if (bool(port.valid) && scalarGroup.contains(robIdentity(port.bits))) {
          scalarWbCount += 1
          scalarLastWb = cycles
        }
      }
      dut.io.robRetire.foreach { port =>
        if (bool(port.valid) && scalarRobRecords.exists(_._1 == robIdentity(port.bits))) scalarRetires += 1
      }
      if (bool(dut.io.huReserve.valid) && bool(dut.io.huReserveReady)) {
        assert(!scalarAccepted && scalarGroup.contains(robIdentity(dut.io.huReserve.bits.robIdx)))
        scalarAccepted = true
        val first = scalarMembers.head
        assert(fetchIdentity(dut.io.huReserve.bits) == first, "HU must recover the first original member")
        dut.io.huReserve.bits.isRVC.expect((!scalarFusionExpected).B)
        dut.io.robHead.sealedGroup.expect(true.B)
        dut.io.robHead.writebacked.expect(true.B)
        dut.io.robHead.interruptSafe.expect(true.B)
        assert(scalarRetires == 0 && cycles > scalarLastWb)
        val group = scalarRobRecords.filter(record => scalarGroup.contains(record._1))
        val renamedFirst = scalarRenameRecords.filter(_.original == scalarMembers.head)
        assert(renamedFirst.size == 1)
        if (scalarFusionExpected) {
          assert(scalarFusionCount == 1 && group.size == 1 && scalarWbCount == 1)
          assert(renamedFirst.head.fused && !renamedFirst.head.canCompress)
          assert(!scalarRenameRecords.exists(_.original == scalarMembers(1)),
            "The second fused member must be removed by the real fusion decoder")
          assert(group.head._3 && group.head._4 && group.head._5 == 1 && group.head._7)
        } else {
          assert(scalarFusionCount == 0 && group.size == 2 && scalarWbCount == 2)
          val renamedSecond = scalarRenameRecords.filter(_.original == scalarMembers(1))
          assert(renamedSecond.size == 1)
          val a = renamedFirst.head
          val b = renamedSecond.head
          assert(a.rob == b.rob && a.size == 2 && b.size == 2 && a.canCompress && b.canCompress)
          assert(!a.needsRob && b.needsRob && a.first && !a.last && !b.first && b.last)
          assert(a.offset == first.offset && b.offset == first.offset && !a.fused && !b.fused)
          assert(group.forall(record => record._2 == first.offset && record._5 == 2 && record._6 == 2 && !record._7))
        }
      }
    }

    // Each original member is checked again architecturally after the handler redirect.
    def scalarBoundaries(): Unit = {
      Seq(false, true).foreach { fused =>
        scalarActive = false
        scalarMembers = Seq.empty
        resetMatrixScenario(if (fused) "scalar-fusion-first-pc" else "scalar-compression-first-pc")
        matrixObservationsEnabled = true
        enterUser()
        execute(addi(5, 0, 9))
        execute(addi(6, 0, 10))
        execute(addi(7, 0, 11))
        execute(addi(10, 0, 7))
        dut.io.fusionEnabled.expect(true.B)
        write(0x800, 1)
        until("natural HU before the same-FTQ packet") {
          bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.irToHU)
        }
        scalarFusionExpected = fused
        scalarActive = true
        scalarAccepted = false
        scalarGroup = None
        scalarRenameRecords.clear()
        scalarRobRecords.clear()
        scalarFusionCount = 0
        scalarWbCount = 0
        scalarLastWb = -1
        scalarRetires = 0
        val firstPC = BigInt(if (fused) 0x1382 else 0x1286)
        val members = if (fused) {
          val rightShift = checkedInstruction((BigInt(32) << 20) | (BigInt(5) << 15) |
            (BigInt(5) << 12) | (BigInt(5) << 7) | 0x13, Instructions.SRLI)
          Seq((slli(5, 10, 32), 4), (rightShift, 4), (addi(7, 0, 5), 4))
        } else Seq((addi(5, 0, 3), 2), (addi(6, 0, 4), 4), (addi(7, 0, 5), 4))
        val ids = enqueueScalarPacket(members, firstPC)
        until("same-FTQ first-PC HU target") {
          scalarAccepted && entries == 1 && releases == 1 &&
            frontendRedirects.exists(r => r.identity == ids.head && r.fullTarget == 0x2000)
        }
        dut.io.uepc.expect(firstPC.U)
        assert(scalarRetires == 0 && !ids.exists(committed))
        assert(reservations == 1 && requests == 1 && completions == 1 && consumes == 1)
        scalarActive = false
        ordinaryPC = 0x2000
        Seq(5 -> 9, 6 -> 10, 7 -> 11).foreach { case (register, expected) =>
          execute((BigInt(0x040) << 20) | (BigInt(register) << 15) | (BigInt(1) << 12) | 0x73)
          dut.io.userScratch.expect(expected.U)
        }
        finishMatrixScenario()
        record("scalar-boundary-result", "\"fusion\":" + fused + ",\"group_writebacks\":" +
          scalarWbCount + ",\"member_retirements\":" + scalarRetires)
      }
    }

    // Observe the actual privilege/debug transition so driver-side retirement waits
    // cannot conceal a stale candidate or shorten the complete return delay.
    private class QualifierRoundTripMonitor extends PriorityMonitor {
      var blockedHU = false
      private var preservePending = false
      private var savedBank = Seq.empty[BigInt]
      private var returning = false
      private var firstEligibleCycle: Option[Int] = None
      private var observedStages = 0
      private var observedRob = false

      def protectPending(): Unit = {
        preservePending = true
        savedBank = Seq(dut.io.ustatus, dut.io.uepc, dut.io.ucause, dut.io.utval).map(uint)
      }
      def armFreshReturn(): Unit = {
        assert(uint(dut.io.mode) != 0 || bool(dut.io.debugMode))
        assert(currentCandidate.filterMask == 0 && !currentCandidate.robHU)
        blockedHU = false
        returning = true
        firstEligibleCycle = None
        observedStages = 0
        observedRob = false
      }
      override def beforeEdge(): Unit = {
        super.beforeEdge()
        val sample = currentCandidate
        if (blockedHU) {
          assert(sample.kill && sample.filterMask == 0 && !sample.robHU)
          assert(!huStage(dut.io.candidate) && !bool(dut.io.entryEffect))
        }
        if (returning) {
          firstEligibleCycle.foreach { start =>
            for (stage <- 0 until 6 if (sample.filterMask & (1 << stage)) != 0) {
              assert(cycles >= start + 1 + stage,
                s"Old HU candidate resurfaced in stage $stage after return")
            }
            observedStages |= sample.filterMask
            if (sample.robHU) {
              assert(cycles >= start + 7 && observedStages == 0x3f)
              observedRob = true
            }
          }
          if (bool(dut.io.huReserve.valid) && bool(dut.io.huReserveReady)) {
            assert(firstEligibleCycle.nonEmpty && observedStages == 0x3f && observedRob)
            record("roundtrip-fresh-candidate-accepted", s"\"restored_cycle\":${firstEligibleCycle.get},\"stages\":$observedStages")
          }
        }
        if (preservePending) {
          dut.io.timerPending.expect(true.B)
          dut.io.handler.expect(false.B)
          assert(entries == 0 && consumes == 0)
          assert(Seq(dut.io.ustatus, dut.io.uepc, dut.io.ucause, dut.io.utval).map(uint) == savedBank)
          if (bool(dut.io.entryEffect)) {
            assert(returning && firstEligibleCycle.nonEmpty && observedStages == 0x3f && observedRob)
            preservePending = false
          } else dut.io.timerConsume.expect(false.B)
        }
      }
      override def afterEdge(): Unit = {
        super.afterEdge()
        if (returning && firstEligibleCycle.isEmpty && uint(dut.io.mode) == 0 && !bool(dut.io.debugMode)) {
          firstEligibleCycle = Some(cycles)
          assert(currentCandidate.filterMask == 0 && !currentCandidate.robHU,
            "The real return edge must not restore an old HU candidate")
          record("roundtrip-qualification-restored", s"\"cycle\":$cycles")
        }
      }
      def verifyFreshEntry(): Unit = {
        assert(firstEligibleCycle.nonEmpty && observedStages == 0x3f && observedRob)
        assert(!preservePending && entries == 1 && consumes == 1)
      }
    }

    private def startQualifierRoundTrip(name: String): QualifierRoundTripMonitor = {
      resetMatrixScenario(name)
      val monitor = new QualifierRoundTripMonitor
      activePriorityMonitor = Some(monitor)
      matrixObservationsEnabled = true
      monitor
    }

    private def expectPendingWithoutEntry(): Unit = {
      dut.io.timerPending.expect(true.B)
      dut.io.timerRemaining.expect(0.U)
      dut.io.handler.expect(false.B)
      dut.io.ustatus.expect(1.U)
      dut.io.uie.expect(0x10.U)
      assert(reservations == 0 && requests == 0 && completions == 0 && releases == 0)
      assert(entries == 0 && consumes == 0)
    }

    private def pendingEcallToMachine(pc: BigInt, expectedEntries: Int, monitor: PriorityMonitor): Unit = {
      val source = enqueue(BigInt(0x73), pc)
      until("real pending HU ECALL entry into M") {
        monitor.machineEntries == expectedEntries && uint(dut.io.mode) == 3 &&
          frontendRedirects.exists(r => r.identity == source && r.target == 0x3000)
      }
      dut.io.mepc.expect(pc.U)
      dut.io.mcause.expect(8.U)
      assert(synchronousTraps.exists(_._2 == pc))
      assert(currentCandidate.filterMask == 0 && !currentCandidate.robHU)
      expectPendingWithoutEntry()
      ordinaryPC = 0x3000
    }

    private def writeAndReadStateen(address: Int, enabled: Boolean): Unit = {
      write(address, if (enabled) 1 else 0)
      execute(csrReadIntoX3(address))
      execute(x3ToUserScratch)
      assert(uint(dut.io.userScratch).testBit(0) == enabled,
        s"Real CSR readback disagrees with stateen C at 0x${address.toHexString}")
      expectPendingWithoutEntry()
    }

    private def returnToPendingUser(instruction: BigInt, pc: BigInt): Unit = {
      val trapsBefore = synchronousTraps.size
      val source = enqueue(instruction, ordinaryPC)
      until("real xRET target reaches FTQ in HU") {
        uint(dut.io.mode) == 0 && !bool(dut.io.debugMode) &&
          frontendRedirects.exists(r => r.identity == source && r.target == pc)
      }
      assert(synchronousTraps.size == trapsBefore)
      ordinaryPC = pc
    }

    private def finishQualifierEntry(pc: BigInt, monitor: QualifierRoundTripMonitor, compressed: Boolean = false): Unit = {
      until("fresh HU candidate reaches real ROB after return") { currentCandidate.robHU }
      val resumed = enqueue(0x13, pc, compressed = compressed)
      until("single returned-context HU entry reaches FTQ") {
        entries == 1 && releases == 1 &&
          frontendRedirects.exists(r => r.identity == resumed && r.target == 0x2000)
      }
      monitor.verifyFreshEntry()
      dut.io.uepc.expect(pc.U)
      dut.io.ucause.expect(((BigInt(1) << 63) | 4).U)
      dut.io.utval.expect(0.U)
      dut.io.handler.expect(true.B)
      dut.io.timerPending.expect(false.B)
      assert(!committed(resumed))
      assert(timerWrites == 1 && reservations == 1 && requests == 1 && completions == 1 && releases == 1)
      assert(frontendRedirects.count(r => r.identity == resumed && r.target == 0x2000) == 1)
      finishPriorityScenario(monitor)
      assert(entries == 1 && consumes == 1)
    }

    def stateenRoundTrip(address: Int): Unit = {
      require(address == 0x30c || address == 0x10c)
      val monitor = startQualifierRoundTrip(s"stateen-${address.toHexString}-revoke-return-regrant")
      write(0x305, 0x3000)
      enterUser()
      write(0x800, 1)
      until("all HU stages occupied before permission takeover") {
        currentCandidate.filterMask == 0x3f && currentCandidate.robHU
      }
      monitor.protectPending()
      val firstEcallPC = BigInt(0x1260)
      pendingEcallToMachine(firstEcallPC, 1, monitor)
      monitor.blockedHU = true
      writeAndReadStateen(address, enabled = false)
      write(0x341, firstEcallPC + 4)
      write(0x300, 0)
      returnToPendingUser(BigInt("30200073", 16), firstEcallPC + 4)
      dut.io.mode.expect(0.U)
      dut.io.virtualMode.expect(false.B)
      val ticksBefore = qualifiedTicks
      val deniedInstruction = enqueue(0x13, firstEcallPC + 4)
      until("safe HU instruction commits with stateen revoked") { committed(deniedInstruction) }
      idle(16)
      assert(qualifiedTicks > ticksBefore, "Stateen must not freeze qualified HU counting cycles")
      assert(!frontendRedirects.exists(_.identity == deniedInstruction))
      expectPendingWithoutEntry()

      val secondEcallPC = firstEcallPC + 8
      pendingEcallToMachine(secondEcallPC, 2, monitor)
      writeAndReadStateen(address, enabled = true)
      val resumedPC = secondEcallPC + 4
      write(0x341, resumedPC)
      write(0x300, 0)
      monitor.armFreshReturn()
      returnToPendingUser(BigInt("30200073", 16), resumedPC)
      dut.io.mode.expect(0.U)
      dut.io.virtualMode.expect(false.B)
      finishQualifierEntry(resumedPC, monitor)
      assert(monitor.machineEntries == 2 && monitor.hostEntries == 0 && monitor.guestEntries == 0 &&
        monitor.nmiEntries == 0 && monitor.debugEntries == 0)
    }

    def haltreqRoundTrip(): Unit = {
      val monitor = startQualifierRoundTrip("haltreq-revokes-hu-and-dret-requalifies")
      enterUser()
      write(0x800, 1)
      until("all HU stages occupied before haltreq") {
        currentCandidate.filterMask == 0x3f && currentCandidate.robHU
      }
      monitor.protectPending()
      dut.io.interrupts.debug.poke(true.B)
      until("normal haltreq replaces every old HU candidate") {
        currentCandidate.filterMask == 0 && !currentCandidate.robHU &&
          bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.debug)
      }
      dut.io.candidate.bits.criticalDebug.expect(false.B)
      monitor.blockedHU = true
      val interruptedPC = BigInt(0x1276)
      val interrupted = enqueue(0x13, interruptedPC, compressed = true)
      until("normal haltreq enters debug at the precise HU head") {
        monitor.debugEntries == 1 && bool(dut.io.debugMode) &&
          frontendRedirects.exists(r => r.identity == interrupted && r.target == dut.debugEntryAddress)
      }
      dut.io.dpc.expect(interruptedPC.U)
      assert(((uint(dut.io.dcsr) >> 6) & 7) == 3, "Normal haltreq must retain DCSR cause Haltreq")
      assert(!committed(interrupted))
      expectPendingWithoutEntry()
      dut.io.interrupts.debug.poke(false.B)
      idle(12)
      ordinaryPC = dut.debugEntryAddress
      monitor.armFreshReturn()
      returnToPendingUser(BigInt("7b200073", 16), interruptedPC)
      dut.io.mode.expect(0.U)
      dut.io.debugMode.expect(false.B)
      finishQualifierEntry(interruptedPC, monitor, compressed = true)
      assert(monitor.debugEntries == 1 && monitor.machineEntries == 0 && monitor.hostEntries == 0 &&
        monitor.guestEntries == 0 && monitor.nmiEntries == 0)
    }

    def enterUser(mstatusBeforeMret: BigInt = 0): Unit = {
      write(0x30c, 1)
      write(0x10c, 1)
      write(0x005, 0x2000)
      write(0x000, 1)
      write(0x004, 0x10)
      write(0x300, mstatusBeforeMret)
      write(0x341, 0x1200)
      execute(BigInt("30200073", 16))
      dut.io.mode.expect(0.U)
      dut.io.virtualMode.expect(false.B)
      ordinaryPC = 0x1200
    }
  }

  // SRAM definitions belong to one elaboration context; feature configurations use separate JVMs.
  if (configuration == "enabled") {
    it should "deliver a naturally expired timer through the real backend and FTQ exactly once" in {
      run("delivery-natural") { d =>
        d.enterUser()
        val writesBefore = d.timerWrites
        d.write(0x800, 1)
        assert(d.timerWrites == writesBefore + 1)
        d.until("real pending and filtered interrupt") {
          d.bool(d.dut.io.timerPending) && d.bool(d.dut.io.interruptSelected)
        }
        val pc = BigInt(0x1236)
        val identity = d.enqueue(0x13, pc, compressed = true)
        d.until("HU effect and frontend redirect") {
          d.entries == 1 && d.frontendRedirects.exists(r => r.identity == identity && r.target == 0x2000)
        }
        d.dut.io.handler.expect(true.B)
        d.dut.io.mode.expect(0.U)
        d.dut.io.uepc.expect(pc.U)
        d.dut.io.ucause.expect(((BigInt(1) << 63) | 4).U)
        d.dut.io.utval.expect(0.U)
        d.dut.io.ustatus.expect(0x10.U)
        assert(d.lastEntryPC.contains(pc))
        assert(!d.committed(identity), "The interrupted instruction must not retire before HU entry")
        assert(d.consumes == 1)
        val delivered = d.frontendRedirects.filter(_.identity == identity)
        assert(delivered.size == 1 && !delivered.head.iaf && !delivered.head.ipf && !delivered.head.igpf)
        d.idle(32)
        assert(d.entries == 1 && d.consumes == 1)
        assert(d.reservations == 1 && d.requests == 1 && d.completions == 1 && d.releases == 1)
        d.dut.io.timerPending.expect(false.B)
        d.dut.io.timerRemaining.expect(0.U)
      }
    }

    it should "invalidate every old candidate stage and preserve automatic return and trace ownership" in {
      run("delivery-cancel-lifecycle-trace") { d =>
        d.candidateCancellationMatrix()
        d.automaticReloadReturn()
        d.traceAfterEntryMatrix()
        d.traceCollisionWithNmi()
        d.finishMatrixScenario()
      }
    }

    Seq(
      "rvv-add-whole", "rvv-sat-unretired", "safe-fp-exception-over-hu",
      "machine-host-user-order", "guest-vs-host-cancel", "nmi31-pulse-held-hu", "nmi43-pulse-held-hu",
      "high-half-satp-8", "high-half-satp-9", "bad-utvec-satp-0", "bad-utvec-satp-8", "bad-utvec-satp-9"
    ).foreach { name =>
      it should s"preserve priority and address ownership: $name" in {
        run(s"delivery-$name") { d => d.priorityScenario(name) }
      }
    }

    it should "use the real commit watchdog for critical debug during a reserved HU event" in {
      run("delivery-watchdog-extended") { d => d.extendedWatchdog() }
    }

    Seq(0x30c, 0x10c).foreach { address =>
      it should s"requalify pending HU after real stateen 0x${address.toHexString} revoke and return" in {
        run(s"delivery-stateen-${address.toHexString}-roundtrip") { d => d.stateenRoundTrip(address) }
      }
    }

    it should "requalify pending HU after normal haltreq and real DRET" in {
      run("delivery-haltreq-roundtrip") { d => d.haltreqRoundTrip() }
    }




    it should "recover the first scalar instruction of real compressed and fused ROB groups" in {
      run("delivery-scalar-group-boundaries") { d => d.scalarBoundaries() }
    }
    it should "receive real watchdog critical debug before a reserved HU PC arrives" in {
      run("delivery-watchdog-before-pc") { d => d.watchdogBeforeReservedPC() }
    }

    it should "defer real watchdog critical debug after HU entry until a fresh handler boundary" in {
      run("delivery-watchdog-after-effect") { d => d.watchdogAfterEntryBeforeRelease() }
    }
  } else {
    it should "elaborate and execute the disabled production backend without user timer delivery" in {
      run("delivery-disabled-smoke", enabled = false) { d => d.disabledBackendSmoke() }
    }
  }
}
