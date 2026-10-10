// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import io.circe.Json
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.scalatest.flatspec.AnyFlatSpec

class InterruptFilterAlignmentTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "InterruptFilter sampled M and HS selection"

  private def mask(causes: Int*): BigInt = causes.foldLeft(BigInt(0))((bits, cause) => bits.setBit(cause))
  private def priorities(entries: (Int, Int)*): BigInt =
    entries.foldLeft(BigInt(0)) { case (bits, (cause, priority)) => bits | (BigInt(priority) << (cause * 8)) }

  private case class Sample(mip: BigInt = 0, mie: BigInt = 0, mideleg: BigInt = 0,
    sip: BigInt = 0, sie: BigInt = 0, hip: BigInt = 0, hie: BigInt = 0, hideleg: BigInt = 0,
    miprios: BigInt = 0, hsiprios: BigInt = 0, mode: Int = 0, virtualMode: Boolean = false,
    mieGlobal: Boolean = false, sieGlobal: Boolean = false,
    debug: Boolean = false, debugMode: Boolean = false, singleStep: Boolean = false, stepIE: Boolean = false,
    nmi: Boolean = false, nmiVector: BigInt = 0, nmie: Boolean = true,
    hu: Boolean = false, huKill: Boolean = false, huClaim: Boolean = false,
    critical: Boolean = false, ceTrigger: Boolean = false, criticalInFlight: Boolean = false,
    criticalClaim: Boolean = false, nmiInFlightValid: Boolean = false, nmiInFlightCause: Int = 0,
    nmiClaimValid: Boolean = false, nmiClaimCause: Int = 0)

  private case class Winner(cause: Int = 0, priority: Int = 0)
  private case class Event(valid: Boolean = false, cause: Int = 0, debug: Boolean = false,
    nmi: Boolean = false, hs: Boolean = false, hu: Boolean = false, critical: Boolean = false)

  private class Driver(dut: InterruptFilterAlignmentHarness, enabled: Boolean, root: Path) {
    private val trace = Files.newBufferedWriter(root.resolve("interrupt-alignment-events.jsonl"), StandardCharsets.UTF_8)
    private var group = "reset"
    private var cycle = 0
    private var checks = 0
    private var topM = Winner()
    private var topHS = Winner()
    private var pipeline = Vector.fill(6)(Event())
    private var completed = Vector.empty[String]
    private var current = Sample()

    // The exercised architectural causes have this fixed default ordering.
    // Sorting software tuples is independent of the RTL's two-level tournament.
    private val architecturalOrder = Vector(11, 3, 7, 9, 1, 5, 12, 10, 2, 6, 13)
    private def winner(pending: BigInt, priority: BigInt, external: Int): Winner = {
      val active = architecturalOrder.filter(pending.testBit)
      require((pending & ~mask(architecturalOrder: _*)) == 0, "The oracle only covers the exercised architectural causes")
      require(!pending.testBit(external), "External IMSIC priority selection is outside this fixture")
      if (active.isEmpty) Winner()
      else {
        def rank(cause: Int): (Int, Int, Int) = {
          val value = ((priority >> (8 * cause)) & 255).toInt
          val index = architecturalOrder.indexOf(cause)
          val category = if (value != 0) 1 else if (index < architecturalOrder.indexOf(external)) 0 else 2
          (category, value, index)
        }
        val cause = active.minBy(rank)
        val value = ((priority >> (8 * cause)) & 255).toInt
        Winner(cause, if (value != 0) value else if (rank(cause)._1 == 0) 0 else 255)
      }
    }
    private def gatherM(s: Sample): BigInt = s.mip & s.mie & ~s.mideleg
    private def gatherHS(s: Sample): BigInt = (s.sip | s.hip) & (s.sie | s.hie) & ~s.hideleg
    private def eligibleM(s: Sample): Boolean = s.mode < 3 || s.mieGlobal
    private def eligibleHS(s: Sample): Boolean = s.virtualMode || s.mode == 0 || (s.mode == 1 && s.sieGlobal)
    private def debugAllowed(s: Sample): Boolean = !s.debugMode && (!s.singleStep || s.stepIE)
    private def normalAllowed(s: Sample): Boolean = debugAllowed(s) && s.nmie
    private def criticalSelected(s: Sample): Boolean = enabled && s.critical && s.ceTrigger && !s.debugMode
    private def ordinary(s: Sample): Event = {
      val m = if (eligibleM(s)) topM.cause else 0
      val hs = if (eligibleHS(s)) topHS.cause else 0
      val destinationHS = m == 0 && hs != 0
      val nmiCause = if (s.nmiVector.testBit(43)) 43 else if (s.nmiVector.testBit(31)) 31 else 0
      val cause = if (!normalAllowed(s)) 0 else if (s.nmi) nmiCause else if (m != 0) m else hs
      val debug = s.debug && debugAllowed(s)
      Event(cause != 0 || debug, cause, debug, s.nmi, destinationHS && !s.nmi)
    }
    private def higher(s: Sample): Boolean = {
      val live = normalAllowed(s) && !s.nmi &&
        ((eligibleM(s) && gatherM(s) != 0) || (eligibleHS(s) && gatherHS(s) != 0))
      ordinary(s).valid || criticalSelected(s) || live
    }
    private def raw(s: Sample): Event = {
      if (!enabled) ordinary(s)
      else if (criticalSelected(s)) Event(valid = true, debug = true, critical = true)
      else if (s.hu && !s.huKill && !higher(s)) Event(valid = true, cause = 4, hu = true)
      else ordinary(s)
    }
    private def revoked(e: Event, s: Sample): Boolean = enabled &&
      ((e.hu && s.huKill) || (e.critical && (!criticalSelected(s) || s.criticalInFlight)) ||
        (e.nmi && !e.debug && s.nmiInFlightValid && e.cause == s.nmiInFlightCause))
    private def claimed(e: Event, s: Sample): Boolean = enabled &&
      ((e.hu && s.huClaim) || (e.critical && s.criticalClaim) ||
        (e.nmi && !e.debug && s.nmiClaimValid && e.cause == s.nmiClaimCause))

    private def drive(s: Sample): Unit = {
      current = s
      val i = dut.io.in
      i.mip.poke(s.mip.U); i.mie.poke(s.mie.U); i.mideleg.poke(s.mideleg.U)
      i.sip.poke(s.sip.U); i.sie.poke(s.sie.U); i.hip.poke(s.hip.U); i.hie.poke(s.hie.U)
      i.hideleg.poke(s.hideleg.U); i.miprios.poke(s.miprios.U); i.hsiprios.poke(s.hsiprios.U)
      i.mode.poke(s.mode.U); i.virtualMode.poke(s.virtualMode.B)
      i.mieGlobal.poke(s.mieGlobal.B); i.sieGlobal.poke(s.sieGlobal.B)
      i.debug.poke(s.debug.B); i.debugMode.poke(s.debugMode.B)
      i.singleStep.poke(s.singleStep.B); i.stepIE.poke(s.stepIE.B)
      i.nmi.poke(s.nmi.B); i.nmiVector.poke(s.nmiVector.U); i.nmie.poke(s.nmie.B)
      i.hu.poke(s.hu.B); i.huKill.poke(s.huKill.B); i.huClaim.poke(s.huClaim.B)
      i.critical.poke(s.critical.B); i.ceTrigger.poke(s.ceTrigger.B)
      i.criticalInFlight.poke(s.criticalInFlight.B); i.criticalClaim.poke(s.criticalClaim.B)
      i.nmiInFlightValid.poke(s.nmiInFlightValid.B); i.nmiInFlightCause.poke(s.nmiInFlightCause.U)
      i.nmiClaimValid.poke(s.nmiClaimValid.B); i.nmiClaimCause.poke(s.nmiClaimCause.U)
    }
    private def uint(name: String, port: UInt, expected: Int): Unit = {
      val actual = port.peek().litValue
      assert(actual == expected, s"$group cycle=$cycle $name expected=$expected actual=$actual")
      checks += 1
    }
    private def bool(name: String, port: Bool, expected: Boolean): Unit = {
      val actual = port.peek().litToBoolean
      assert(actual == expected, s"$group cycle=$cycle $name expected=$expected actual=$actual")
      checks += 1
    }
    private def check(s: Sample): Unit = {
      val o = dut.io.out
      uint("mtopi.IID", dut.io.mtopiIID, topM.cause)
      uint("mtopi.IPRIO", dut.io.mtopiPriority, topM.priority)
      uint("stopi.IID", dut.io.stopiIID, topHS.cause)
      uint("stopi.IPRIO", dut.io.stopiPriority, topHS.priority)
      val selected = pipeline.last
      val valid = selected.valid && !revoked(selected, s)
      bool("interruptVec.valid", o.interruptVec.valid, valid)
      if (valid) {
        uint("interruptVec.bits", o.interruptVec.bits, selected.cause)
        bool("debug", o.debug, selected.debug); bool("nmi", o.nmi, selected.nmi)
        bool("irToHS", o.irToHS, selected.hs); bool("irToVS", o.irToVS, false)
        bool("virtualInterruptIsHvictlInject", o.virtualInterruptIsHvictlInject, false)
      }
      if (enabled) {
        bool("higherPriority", o.higherPriority.get, higher(s))
        val candidate = o.candidate.get
        bool("candidate.valid", candidate.valid, valid)
        if (valid) {
          uint("candidate.cause", candidate.bits.cause, selected.cause)
          bool("candidate.debug", candidate.bits.debug, selected.debug)
          bool("candidate.nmi", candidate.bits.nmi, selected.nmi)
          bool("candidate.irToHS", candidate.bits.irToHS, selected.hs)
          bool("candidate.irToVS", candidate.bits.irToVS, false)
          bool("candidate.irToHU", candidate.bits.irToHU, selected.hu)
          bool("candidate.criticalDebug", candidate.bits.criticalDebug, selected.critical)
          bool("candidate.isInterrupt", candidate.bits.isInterrupt, true)
          bool("candidate.virtualInterruptIsHvictlInject", candidate.bits.virtualInterruptIsHvictlInject, false)
          uint("candidate.hvictlIID", candidate.bits.hvictlIID, 0)
        }
      }
    }
    private def eventJson(e: Event): Json = Json.obj(
      "valid" -> Json.fromBoolean(e.valid), "cause" -> Json.fromInt(e.cause),
      "debug" -> Json.fromBoolean(e.debug), "nmi" -> Json.fromBoolean(e.nmi),
      "hs" -> Json.fromBoolean(e.hs), "hu" -> Json.fromBoolean(e.hu), "critical" -> Json.fromBoolean(e.critical))
    private def record(s: Sample, launched: Event): Unit = {
      trace.write(Json.obj("group" -> Json.fromString(group), "cycle" -> Json.fromInt(cycle),
        "input" -> Json.fromString(s.toString), "expectedM" -> Json.fromInt(topM.cause),
        "expectedHS" -> Json.fromInt(topHS.cause), "launch" -> eventJson(launched),
        "selected" -> eventJson(pipeline.last), "higherPriority" -> Json.fromBoolean(higher(s))).noSpaces)
      trace.newLine()
    }
    private def tick(s: Sample = Sample()): Unit = {
      drive(s)
      // Check combinational changes before the edge as well as registered output.
      check(s)
      val launched = raw(s)
      pipeline = (launched +: pipeline.dropRight(1)).map { e =>
        if (revoked(e, s) || claimed(e, s)) e.copy(valid = false) else e
      }
      topM = winner(gatherM(s), s.miprios, 11)
      topHS = winner(gatherHS(s), s.hsiprios, 9)
      dut.clock.step(); cycle += 1
      check(s)
      record(s, launched)
    }
    private def hold(s: Sample, clocks: Int): Unit = (0 until clocks).foreach(_ => tick(s))
    private def reset(s: Sample = Sample()): Unit = {
      drive(s); dut.reset.poke(true.B); dut.clock.step(8); cycle += 8
      topM = Winner(); topHS = Winner(); pipeline = Vector.fill(6)(Event())
      dut.reset.poke(false.B)
      check(s)
    }
    private def runGroup(name: String)(body: => Unit): Unit = {
      group = name; reset(); body; hold(Sample(), 10)
      completed :+= name
      trace.flush()
      println(s"INTERRUPT_ALIGNMENT_GROUP_PASS group=$group cycle=$cycle checks=$checks")
    }

    def run(): Unit = {
      val m = Sample(mip = mask(7), mie = mask(3, 7), mode = 0)
      val hs = Sample(sip = mask(5), sie = mask(1, 5), mode = 0)
      runGroup("m-level-rise-and-fall") { hold(m, 4) }
      runGroup("hs-level-rise-and-fall") { hold(hs, 4) }
      runGroup("m-single-cycle-pulse") { tick(m) }
      runGroup("hs-single-cycle-pulse") { tick(hs) }
      runGroup("m-continuous-distinct-winners") {
        (0 until 16).foreach(n => tick(m.copy(mip = mask(if (n % 2 == 0) 3 else 7))))
      }
      runGroup("hs-continuous-distinct-winners") {
        (0 until 16).foreach(n => tick(hs.copy(sip = mask(if (n % 2 == 0) 1 else 5))))
      }
      runGroup("m-priority-and-tie-transitions") {
        Seq((0, 0), (4, 1), (1, 4), (3, 3), (255, 1), (1, 255), (0, 1), (1, 0)).foreach {
          case (software, timer) => tick(m.copy(mip = mask(3, 7), miprios = priorities(3 -> software, 7 -> timer)))
        }
      }
      runGroup("hs-priority-and-tie-transitions") {
        Seq((0, 0), (4, 1), (1, 4), (3, 3), (255, 1), (1, 255), (0, 1), (1, 0)).foreach {
          case (software, timer) => tick(hs.copy(sip = mask(1, 5), hsiprios = priorities(1 -> software, 5 -> timer)))
        }
      }
      runGroup("m-enable-and-delegation-transitions") {
        val both = m.copy(mip = mask(1, 7), mie = mask(1, 7), miprios = priorities(1 -> 1, 7 -> 2))
        tick(both); tick(both.copy(mie = 0)); tick(both)
        tick(both.copy(mideleg = mask(1))); tick(both); tick(both.copy(mie = mask(1), mideleg = mask(1)))
      }
      runGroup("hs-enable-hypervisor-and-delegation-transitions") {
        val both = hs.copy(sip = mask(1), sie = mask(1), hip = mask(2), hie = mask(2),
          hsiprios = priorities(1 -> 2, 2 -> 1))
        tick(both); tick(both.copy(hie = 0)); tick(both); tick(both.copy(hideleg = mask(2)))
        tick(both.copy(sie = 0)); tick(both.copy(sie = 0, hideleg = mask(2)))
      }
      runGroup("mode-and-global-enable-boundaries") {
        val both = m.copy(sip = hs.sip, sie = hs.sie)
        hold(both.copy(mode = 3), 3)
        tick(both.copy(mode = 3, mieGlobal = true)); tick(both.copy(mode = 3))
        tick(both.copy(mode = 1)); tick(both.copy(mode = 0))
        hold(hs.copy(mode = 1), 3)
        tick(hs.copy(mode = 1, sieGlobal = true)); tick(hs.copy(mode = 1)); tick(hs)
      }
      runGroup("host-interrupts-from-virtual-modes") {
        hold(m.copy(mode = 1, virtualMode = true), 2); tick(m.copy(virtualMode = true))
        hold(hs.copy(mode = 1, virtualMode = true), 2); tick(hs.copy(virtualMode = true))
      }
      runGroup("debug-step-and-nmie-gates") {
        hold(m, 2); tick(m.copy(nmie = false)); tick(m.copy(debugMode = true))
        tick(m.copy(singleStep = true)); tick(m.copy(singleStep = true, stepIE = true)); tick(m)
      }
      runGroup("nmi-pulses-and-source-priority") {
        tick(Sample(nmi = true, nmiVector = mask(31)))
        tick(Sample(nmi = true, nmiVector = mask(31, 43)))
        tick(Sample(nmi = true, nmiVector = mask(43)))
        tick(Sample(nmi = true, nmiVector = mask(43), nmie = false))
      }
      runGroup("debug-request-masks-and-nmi-coexistence") {
        tick(Sample(debug = true)); tick(Sample(debug = true, nmie = false))
        tick(Sample(debug = true, debugMode = true)); tick(Sample(debug = true, singleStep = true))
        tick(Sample(debug = true, singleStep = true, stepIE = true))
        tick(Sample(debug = true, nmi = true, nmiVector = mask(43)))
      }
      runGroup("reset-with-sorted-and-pipelined-interrupts") {
        hold(m, 9); reset(m); tick(m); tick(m.copy(mip = 0))
      }
      if (enabled) {
        runGroup("hu-blocked-immediately-by-new-m-and-hs-pending") {
          hold(Sample(hu = true), 8)
          tick(m.copy(hu = true, huKill = true)); tick(m.copy(hu = true, huKill = true, mip = 0))
          hold(Sample(hu = true), 9)
          tick(hs.copy(hu = true, huKill = true)); tick(hs.copy(hu = true, huKill = true, sip = 0))
          hold(Sample(hu = true), 9)
        }
        runGroup("hu-claim-and-kill-do-not-resurrect-old-candidates") {
          hold(Sample(hu = true), 8); tick(Sample(hu = true, huClaim = true)); hold(Sample(), 8)
          hold(Sample(hu = true), 8); tick(Sample(huKill = true)); hold(Sample(), 8)
        }
        runGroup("nmi-claim-matches-cause-and-inflight-revokes") {
          val nmi = Sample(nmi = true, nmiVector = mask(43))
          hold(nmi, 8); tick(nmi.copy(nmiClaimValid = true, nmiClaimCause = 31))
          tick(nmi.copy(nmiClaimValid = true, nmiClaimCause = 43))
          hold(Sample(nmiInFlightValid = true, nmiInFlightCause = 43), 8)
          hold(nmi, 8); tick(Sample(nmiInFlightValid = true, nmiInFlightCause = 31))
          tick(Sample(nmiInFlightValid = true, nmiInFlightCause = 43))
        }
        runGroup("critical-eligibility-priority-claim-and-revocation") {
          val critical = Sample(critical = true, ceTrigger = true, singleStep = true, nmie = false,
            debug = true, nmi = true, nmiVector = mask(43), hu = true)
          hold(critical, 8); tick(critical.copy(criticalClaim = true))
          hold(critical.copy(criticalInFlight = true), 8)
          hold(critical, 8); tick(critical.copy(ceTrigger = false)); hold(Sample(), 8)
          hold(critical, 8); tick(critical.copy(debugMode = true)); hold(Sample(), 8)
        }
      }
      runGroup("deterministic-consecutive-input-stress") {
        val random = new scala.util.Random(0x1274)
        (0 until 192).foreach { _ =>
          val mode = Vector(0, 1, 3)(random.nextInt(3))
          tick(Sample(mip = Vector(mask(3), mask(7), mask(3, 7), BigInt(0))(random.nextInt(4)),
            mie = Vector(mask(3), mask(7), mask(3, 7), BigInt(0))(random.nextInt(4)),
            sip = Vector(mask(1), mask(5), mask(1, 5), BigInt(0))(random.nextInt(4)), sie = mask(1, 5),
            miprios = priorities(3 -> random.nextInt(4), 7 -> random.nextInt(4)),
            hsiprios = priorities(1 -> random.nextInt(4), 5 -> random.nextInt(4)),
            mode = mode, virtualMode = mode != 3 && random.nextBoolean(),
            mieGlobal = random.nextBoolean(), sieGlobal = random.nextBoolean(),
            nmie = random.nextInt(4) != 0, singleStep = random.nextInt(5) == 0,
            stepIE = random.nextBoolean(), debug = random.nextInt(7) == 0))
        }
      }
    }
    def finish(error: Option[Throwable]): Unit = {
      trace.close()
      val result = Json.obj("status" -> Json.fromString(if (error.isEmpty) "PASS" else "FAIL"),
        "enabled" -> Json.fromBoolean(enabled), "cycles" -> Json.fromInt(cycle), "checks" -> Json.fromInt(checks),
        "groupCount" -> Json.fromInt(completed.size), "completedGroups" -> Json.arr(completed.map(Json.fromString): _*),
        "lastGroup" -> Json.fromString(group), "lastInput" -> Json.fromString(current.toString),
        "error" -> error.map(e => Json.fromString(e.toString)).getOrElse(Json.Null),
        "scope" -> Json.fromString("Real InterruptFilter public inputs/outputs; sampled M/HS arbitration and six candidate stages. VS arbitration, IMSIC external priority, CSR ownership and ROB acceptance require their separate tests."))
      Files.write(root.resolve("interrupt-alignment-result.json"), (result.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
    }
  }

  it should "align sampled validity and cause while preserving candidate priority and cancellation" in {
    val root = Paths.get(sys.env("FDI_CONTROL_RUN_ROOT")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated interrupt evidence directory")
    require(!Files.exists(root.resolve("interrupt-alignment-events.jsonl")), "Keep each execution's evidence separate")
    val enabled = sys.env("FDI_CONTROL_ENABLED") match {
      case "true" | "1" | "on" => true
      case "false" | "0" | "off" => false
      case other => throw new IllegalArgumentException(s"Invalid FDI_CONTROL_ENABLED: $other")
    }
    test(new InterruptFilterAlignmentHarness(enabled)).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("compiled-interrupt"))) { dut =>
      val driver = new Driver(dut, enabled, root)
      var error: Option[Throwable] = None
      try driver.run()
      catch { case failure: Throwable => error = Some(failure); throw failure }
      finally driver.finish(error)
    }
  }
}
