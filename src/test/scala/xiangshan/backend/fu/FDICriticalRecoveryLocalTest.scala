// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import io.circe.Json
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.fu.NewCSR.CSREvents._

class FDICriticalRecoveryLocalTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Critical recovery public CSR boundaries"

  private case class Owner(rob: Int, ftq: Int, offset: Int = 1, flag: Boolean = false, rvc: Boolean = false)
  private case class Event(owner: Owner, critical: Boolean = false, halt: Boolean = false, hu: Boolean = false)
  private val debugTarget = BigInt("38020800", 16)
  private val handlerTarget = BigInt("80002000", 16)
  private val userPC = BigInt("80001236", 16)

  private class Driver(val dut: FDICriticalRecoveryLocalHarness, val enabled: Boolean, root: Path) {
    private val trace = Files.newBufferedWriter(root.resolve("critical-local-events.jsonl"), StandardCharsets.UTF_8)
    private var group = "initial"
    private var cycle = 0
    private var debugEffects = 0
    private var scratchWrites = 0
    private var returnPCWrites = 0
    private var huEffects = 0
    private var reservations = 0
    private var handoffs = 0
    private var terminals = 0
    private var releases = 0
    private var claims = 0
    private var requests = 0
    private var responses = 0
    private var distributions = Map.empty[Int, Int].withDefaultValue(0)
    private var completed = Vector.empty[Json]
    private var witnesses = Vector.empty[Json]
    private def bool(x: Bool): Boolean = x.peek().litToBoolean
    private def uint(x: UInt): BigInt = x.peek().litValue
    private def countJson: Json = Json.obj(
      "debugEffects" -> Json.fromInt(debugEffects), "scratchWrites" -> Json.fromInt(scratchWrites),
      "returnPCWrites" -> Json.fromInt(returnPCWrites), "huEffects" -> Json.fromInt(huEffects),
      "reservations" -> Json.fromInt(reservations), "pcHandoffs" -> Json.fromInt(handoffs),
      "terminals" -> Json.fromInt(terminals), "releases" -> Json.fromInt(releases),
      "claims" -> Json.fromInt(claims), "requests" -> Json.fromInt(requests),
      "responses" -> Json.fromInt(responses))
    private def record(event: String, details: (String, Json)*): Unit = {
      trace.write(Json.obj((Seq("group" -> Json.fromString(group), "cycle" -> Json.fromInt(cycle),
        "event" -> Json.fromString(event)) ++ details): _*).noSpaces)
      trace.newLine()
    }
    private def ownerJson(e: InterruptEventIdentity): Json = Json.obj(
      "rob" -> Json.fromBigInt(uint(e.robIdx.value)), "robFlag" -> Json.fromBoolean(bool(e.robIdx.flag)),
      "ftq" -> Json.fromBigInt(uint(e.ftqIdx.value)), "ftqFlag" -> Json.fromBoolean(bool(e.ftqIdx.flag)),
      "offset" -> Json.fromBigInt(uint(e.ftqOffset)), "rvc" -> Json.fromBoolean(bool(e.isRVC)),
      "critical" -> Json.fromBoolean(bool(e.interrupt.criticalDebug)),
      "halt" -> Json.fromBoolean(bool(e.interrupt.debug) && !bool(e.interrupt.criticalDebug)),
      "hu" -> Json.fromBoolean(bool(e.interrupt.irToHU)))

    def edge(): Unit = {
      if (bool(dut.observed.debugEffect)) {
        dut.observed.targetValid.expect(true.B)
        dut.observed.target.pc.expect(debugTarget.U)
        dut.observed.target.raiseIPF.expect(false.B)
        dut.observed.target.raiseIAF.expect(false.B)
        dut.observed.target.raiseIGPF.expect(false.B)
        debugEffects += 1
        record("debug-effect", "target" -> Json.fromBigInt(uint(dut.observed.target.pc)),
          "targetValid" -> Json.fromBoolean(bool(dut.observed.targetValid)),
          "sourcePrivilege" -> Json.fromBigInt(uint(dut.observed.mode)),
          "sourceVirtual" -> Json.fromBoolean(bool(dut.observed.virtualMode)))
      }
      if (bool(dut.observed.scratchWrite)) { scratchWrites += 1; record("scratch-write") }
      if (bool(dut.observed.returnPCWrite)) { returnPCWrites += 1; record("return-pc-write") }
      if (bool(dut.observed.entryEffect)) { huEffects += 1; record("hu-effect") }
      if (bool(dut.io.request.valid) && bool(dut.io.request.ready)) {
        requests += 1
        record("csr-accept", "instruction" -> Json.fromBigInt(uint(dut.io.request.bits.instruction)),
          "source" -> Json.fromBigInt(uint(dut.io.request.bits.source)), "owner" -> ownerJson(dut.io.request.bits.owner))
      }
      if (bool(dut.io.response.valid) && bool(dut.io.response.ready)) {
        responses += 1
        record("csr-response", "rob" -> Json.fromBigInt(uint(dut.io.response.bits.rob.value)),
          "flag" -> Json.fromBoolean(bool(dut.io.response.bits.rob.flag)),
          "data" -> Json.fromBigInt(uint(dut.io.response.bits.data)))
      }
      if (bool(dut.io.distribution.w.valid)) {
        val address = uint(dut.io.distribution.w.bits.addr).toInt
        distributions += address -> (distributions(address) + 1)
        record("distribution", "address" -> Json.fromInt(address),
          "data" -> Json.fromBigInt(uint(dut.io.distribution.w.bits.data)))
      }
      if (bool(dut.io.accepted.valid)) { claims += 1; record("accepted-owner", "owner" -> ownerJson(dut.io.accepted.bits)) }
      if (bool(dut.io.hu.reserve.valid) && bool(dut.io.hu.reserve.ready)) {
        reservations += 1; record("hu-reserve", "owner" -> ownerJson(dut.io.hu.reserve.bits))
      }
      if (bool(dut.io.hu.request.valid) && bool(dut.io.hu.request.ready)) {
        handoffs += 1
        record("hu-pc", "owner" -> ownerJson(dut.io.hu.request.bits.event),
          "pc" -> Json.fromBigInt(uint(dut.io.hu.request.bits.pc)))
      }
      if (bool(dut.io.hu.completion.valid) && bool(dut.io.hu.completion.ready)) {
        terminals += 1
        record("hu-terminal", "owner" -> ownerJson(dut.io.hu.completion.bits.event),
          "outcome" -> Json.fromBigInt(uint(dut.io.hu.completion.bits.outcome)),
          "target" -> Json.fromBigInt(uint(dut.io.hu.completion.bits.target.pc)))
      }
      if (bool(dut.io.hu.release)) { releases += 1; record("hu-release") }
      assert(terminals <= reservations && releases <= terminals)
      dut.clock.step()
      cycle += 1
    }
    private def idle(n: Int): Unit = (0 until n).foreach(_ => edge())
    private def until(label: String, maximum: Int = 32)(condition: => Boolean): Unit = {
      var waited = 0
      while (!condition && waited < maximum) { edge(); waited += 1 }
      assert(condition, s"$group: timed out waiting for $label at cycle $cycle")
    }
    private def event(e: InterruptEventIdentity, value: Event): Unit = {
      e.robIdx.value.poke(value.owner.rob.U); e.robIdx.flag.poke(value.owner.flag.B)
      e.ftqIdx.value.poke(value.owner.ftq.U); e.ftqIdx.flag.poke(value.owner.flag.B)
      e.ftqOffset.poke(value.owner.offset.U); e.isRVC.poke(value.owner.rvc.B)
      e.interrupt.cause.poke((if (value.hu) 4 else 0).U)
      e.interrupt.debug.poke((value.halt || value.critical).B)
      e.interrupt.criticalDebug.poke(value.critical.B)
      e.interrupt.nmi.poke(false.B); e.interrupt.virtualInterruptIsHvictlInject.poke(false.B)
      e.interrupt.irToHS.poke(false.B); e.interrupt.irToVS.poke(false.B)
      e.interrupt.irToHU.poke(value.hu.B); e.interrupt.isInterrupt.poke(true.B)
      e.interrupt.hvictlIID.poke(0.U)
    }
    private def expectEvent(e: InterruptEventIdentity, value: Event): Unit = {
      e.robIdx.value.expect(value.owner.rob.U); e.robIdx.flag.expect(value.owner.flag.B)
      e.ftqIdx.value.expect(value.owner.ftq.U); e.ftqIdx.flag.expect(value.owner.flag.B)
      e.ftqOffset.expect(value.owner.offset.U); e.isRVC.expect(value.owner.rvc.B)
      expectDescriptor(e.interrupt, value)
    }
    private def expectDescriptor(e: InterruptDescriptor, value: Event): Unit = {
      e.cause.expect((if (value.hu) 4 else 0).U)
      e.debug.expect((value.halt || value.critical).B); e.criticalDebug.expect(value.critical.B)
      e.irToHU.expect(value.hu.B); e.nmi.expect(false.B)
      e.irToHS.expect(false.B); e.irToVS.expect(false.B)
      e.virtualInterruptIsHvictlInject.expect(false.B); e.isInterrupt.expect(true.B)
      e.hvictlIID.expect(0.U)
    }
    private def reset(): Unit = {
      dut.io.request.valid.poke(false.B)
      dut.io.request.bits.instruction.poke(0x73.U); dut.io.request.bits.source.poke(0.U)
      event(dut.io.request.bits.owner, Event(Owner(4, 2)))
      dut.io.response.ready.poke(true.B)
      dut.io.critical.poke(false.B); dut.io.halt.poke(false.B)
      dut.io.accepted.valid.poke(false.B); event(dut.io.accepted.bits, Event(Owner(6, 3)))
      dut.io.trap.valid.poke(false.B); dut.io.trap.bits.pc.poke(userPC.U)
      dut.io.trap.bits.singleStep.poke(false.B); dut.io.trap.bits.isInterrupt.poke(false.B)
      event(dut.io.trap.bits.event, Event(Owner(6, 3)))
      dut.io.hu.reserve.valid.poke(false.B); event(dut.io.hu.reserve.bits, Event(Owner(8, 4), hu = true))
      dut.io.hu.request.valid.poke(false.B); dut.io.hu.request.bits.pc.poke(userPC.U)
      event(dut.io.hu.request.bits.event, Event(Owner(8, 4), hu = true))
      dut.io.hu.cancel.valid.poke(false.B); dut.io.hu.cancel.bits.externalRedirect.poke(false.B)
      event(dut.io.hu.cancel.bits.event, Event(Owner(8, 4), hu = true))
      dut.io.hu.completion.ready.poke(false.B); dut.io.hu.release.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(5); cycle += 5
      dut.reset.poke(false.B)
      debugEffects = 0; scratchWrites = 0; returnPCWrites = 0; huEffects = 0
      reservations = 0; handoffs = 0; terminals = 0; releases = 0; claims = 0
      requests = 0; responses = 0; distributions = Map.empty[Int, Int].withDefaultValue(0)
      edge()
      dut.observed.mode.expect(3.U); dut.observed.debugMode.expect(false.B)
      dut.observed.critical.expect(false.B); dut.io.response.valid.expect(false.B)
      dut.io.candidate.valid.expect(false.B); dut.io.hu.completion.valid.expect(false.B)
      record("reset-complete")
    }
    private def runGroup(name: String)(body: => Unit): Unit = {
      group = name; witnesses = Vector.empty; reset(); body
      val result = Json.obj("group" -> Json.fromString(name), "endCycle" -> Json.fromInt(cycle),
        "counts" -> countJson, "witnesses" -> Json.arr(witnesses: _*))
      completed :+= result
      record("group-pass", "counts" -> countJson)
      trace.flush()
      println(s"CRITICAL_LOCAL_GROUP_PASS group=$name cycle=$cycle counts=${countJson.noSpaces}")
    }
    private def checkpoint(name: String): Unit = {
      val witness = Json.obj("name" -> Json.fromString(name), "cycle" -> Json.fromInt(cycle), "counts" -> countJson)
      witnesses :+= witness
      record("checkpoint", "witness" -> witness)
    }
    private def instruction(address: Int, write: Boolean): BigInt =
      (BigInt(address) << 20) | (if (write) BigInt(1) << 15 else BigInt(0)) |
        (BigInt(if (write) 1 else 2) << 12) | (BigInt(1) << 7) | 0x73
    private def begin(inst: BigInt, source: BigInt = 0, ready: Boolean = true,
      owner: Owner = Owner(4, 2)): Unit = {
      dut.io.request.bits.instruction.poke(inst.U); dut.io.request.bits.source.poke(source.U)
      event(dut.io.request.bits.owner, Event(owner))
      dut.io.response.ready.poke(ready.B); dut.io.request.valid.poke(true.B)
      until("CSR acceptance")(bool(dut.io.request.ready)); edge()
      dut.io.request.valid.poke(false.B)
      dut.io.response.valid.expect(true.B)
      dut.io.response.bits.illegal.expect(false.B); dut.io.response.bits.virtualIllegal.expect(false.B)
      dut.io.response.bits.rob.value.expect(owner.rob.U); dut.io.response.bits.rob.flag.expect(owner.flag.B)
    }
    private def access(address: Int, value: Option[BigInt] = None): BigInt = {
      begin(instruction(address, value.nonEmpty), value.getOrElse(BigInt(0)))
      val result = uint(dut.io.response.bits.data)
      edge(); dut.io.response.valid.expect(false.B)
      result
    }
    private def write(address: Int, value: BigInt): Unit = { access(address, Some(value)); () }
    private def system(inst: BigInt): Unit = { begin(inst); edge(); dut.io.response.valid.expect(false.B) }
    private def debugSetup(cetrig: Boolean, step: Boolean = false): Unit = {
      dut.io.trap.bits.singleStep.poke(true.B); dut.io.trap.valid.poke(true.B)
      edge(); dut.io.trap.valid.poke(false.B); dut.io.trap.bits.singleStep.poke(false.B)
      dut.observed.debugMode.expect(true.B)
      write(0x7b0, BigInt(3) | (if (cetrig) BigInt(1) << 19 else BigInt(0)) |
        (if (step) BigInt(1) << 2 else BigInt(0)))
      write(0x7b1, 0x1200)
      system(BigInt("7b200073", 16))
      dut.observed.debugMode.expect(false.B); dut.observed.mode.expect(3.U)
      assert(uint(dut.observed.dcsr).testBit(19) == cetrig)
      assert(uint(dut.observed.dcsr).testBit(2) == step && !uint(dut.observed.dcsr).testBit(11))
    }
    private def pulseCritical(): Unit = {
      dut.io.critical.poke(true.B); record("critical-input"); edge(); dut.io.critical.poke(false.B)
      dut.observed.critical.expect(true.B)
    }
    private def candidate(value: Event): Unit = {
      until("selected candidate") {
        bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.criticalDebug) == value.critical &&
          bool(dut.io.candidate.bits.irToHU) == value.hu && bool(dut.io.candidate.bits.debug) == (value.halt || value.critical)
      }
      expectDescriptor(dut.io.candidate.bits, value)
      record("candidate-checked", "critical" -> Json.fromBoolean(value.critical),
        "halt" -> Json.fromBoolean(value.halt), "hu" -> Json.fromBoolean(value.hu))
    }
    private def claim(value: Event): Unit = {
      candidate(value)
      event(dut.io.accepted.bits, value); dut.io.accepted.valid.poke(true.B)
      edge(); dut.io.accepted.valid.poke(false.B)
    }
    private def delivered(value: Event, pc: BigInt, cause: Int,
      sourcePrivilege: Int = 3, sourceVirtual: Boolean = false): Unit = {
      val before = debugEffects
      dut.observed.mode.expect(sourcePrivilege.U); dut.observed.virtualMode.expect(sourceVirtual.B)
      event(dut.io.trap.bits.event, value)
      dut.io.trap.bits.pc.poke(pc.U); dut.io.trap.bits.isInterrupt.poke(true.B)
      dut.io.trap.valid.poke(true.B); edge()
      dut.io.trap.valid.poke(false.B); dut.io.trap.bits.isInterrupt.poke(false.B)
      until("owned Debug effect")(bool(dut.observed.debugMode))
      assert(debugEffects == before + 1)
      dut.observed.dpc.expect((pc & ~BigInt(1)).U)
      assert(((uint(dut.observed.dcsr) >> 6) & 7) == cause)
      assert((uint(dut.observed.dcsr) & 3) == sourcePrivilege)
      assert(uint(dut.observed.dcsr).testBit(5) == sourceVirtual)
      idle(10)
      assert(debugEffects == before + 1)
      dut.io.candidate.valid.expect(false.B)
      record("owned-debug-checked", "pc" -> Json.fromBigInt(pc), "cause" -> Json.fromInt(cause),
        "sourcePrivilege" -> Json.fromInt(sourcePrivilege), "sourceVirtual" -> Json.fromBoolean(sourceVirtual))
    }
    private def prepareHU(): Unit = {
      debugSetup(cetrig = true)
      write(0x30c, (BigInt(1) << 63) | 1); write(0x60c, (BigInt(1) << 63) | 1); write(0x10c, 1)
      write(0x005, handlerTarget); write(0x000, 1); write(0x004, 0x10)
      write(0x300, 0); write(0x341, userPC); system(BigInt("30200073", 16))
      dut.observed.mode.expect(0.U); dut.observed.virtualMode.expect(false.B)
      write(0x800, 1)
      until("HU timer pending") { (uint(dut.observed.uip) & 0x10) != 0 }
    }
    private def reserve(value: Event): Unit = {
      candidate(value)
      event(dut.io.hu.reserve.bits, value); event(dut.io.accepted.bits, value)
      dut.io.hu.reserve.valid.poke(true.B)
      dut.io.hu.reserve.ready.expect(true.B)
      dut.io.accepted.valid.poke(true.B); edge()
      dut.io.accepted.valid.poke(false.B); dut.io.hu.reserve.valid.poke(false.B)
      assert(reservations == 1)
    }
    private def handoff(value: Event, pc: BigInt): Unit = {
      event(dut.io.hu.request.bits.event, value); dut.io.hu.request.bits.pc.poke(pc.U)
      dut.io.hu.request.valid.poke(true.B)
      until("reserved PC acceptance")(bool(dut.io.hu.request.ready)); edge()
      dut.io.hu.request.valid.poke(false.B)
      assert(handoffs == 1)
    }
    private def checkTerminal(value: Event, outcome: Int, target: BigInt): Unit = {
      dut.io.hu.completion.valid.expect(true.B)
      expectEvent(dut.io.hu.completion.bits.event, value)
      dut.io.hu.completion.bits.outcome.expect(outcome.U)
      dut.io.hu.completion.bits.target.pc.expect(target.U)
      dut.io.hu.completion.bits.target.raiseIPF.expect(false.B)
      dut.io.hu.completion.bits.target.raiseIAF.expect(false.B)
      dut.io.hu.completion.bits.target.raiseIGPF.expect(false.B)
    }
    private def release(): Unit = {
      dut.io.hu.release.poke(true.B); edge(); dut.io.hu.release.poke(false.B)
      assert(releases == 1)
    }

    def run(): Unit = {
      runGroup("ordinary-csr-and-step-control") {
        debugSetup(cetrig = false, step = true)
        val before = debugEffects
        val value = BigInt("13579bdf2468ace0", 16)
        begin(instruction(0x340, write = true), value, ready = false)
        dut.observed.scratchWrite.expect(true.B); edge()
        dut.observed.scratch.expect(value.U)
        dut.io.halt.poke(true.B)
        for (_ <- 0 until 12) {
          dut.io.response.valid.expect(true.B); dut.observed.scratchWrite.expect(false.B)
          dut.observed.scratch.expect(value.U); dut.observed.debugMode.expect(false.B)
          dut.io.nativeInterrupt.expect(false.B); edge()
        }
        assert(scratchWrites == 1 && debugEffects == before)
        dut.io.halt.poke(false.B); dut.io.response.ready.poke(true.B); edge()
        assert(access(0x340) == value)
        if (!enabled) {
          dut.io.candidate.valid.expect(false.B); dut.io.hu.reserve.ready.expect(false.B)
          dut.io.hu.request.ready.expect(false.B); dut.io.hu.effectLocked.expect(false.B)
          assert(returnPCWrites == 0 && huEffects == 0 && reservations == 0)
        }
      }
      runGroup("cetrig-disabled-and-reset") {
        val before = debugEffects
        pulseCritical()
        for (_ <- 0 until 12) {
          dut.io.criticalCommitBlock.expect(true.B); dut.io.candidate.valid.expect(false.B)
          dut.observed.debugMode.expect(false.B); dut.observed.mode.expect(3.U); edge()
        }
        assert(debugEffects == before)
        checkpoint("cetrig-disabled-sticky-without-entry")
        reset(); idle(10)
        assert(debugEffects == 0 && scratchWrites == 0 && returnPCWrites == 0 && claims == 0)
        write(0x340, 0x55); assert(access(0x340) == 0x55)
      }
      if (enabled) {
        runGroup("effected-csr-response-survives-pending-critical") {
          debugSetup(cetrig = true)
          // Native mscratch has no reset value; establish the comparison by software.
          val scratchSeed = BigInt("1357ace0", 16)
          write(0x340, scratchSeed); dut.observed.scratch.expect(scratchSeed.U)
          val scratchBaseline = scratchWrites
          assert(scratchBaseline == 1)
          val before = debugEffects
          val value = BigInt("123456789abcdef0", 16)
          val owner = Owner(12, 7, 2, flag = true)
          begin(instruction(0x8b1, write = true), value, ready = false, owner = owner)
          dut.observed.returnPCWrite.expect(true.B)
          dut.io.distribution.w.valid.expect(true.B)
          dut.io.distribution.w.bits.addr.expect(0x8b1.U)
          dut.io.distribution.w.bits.data.expect(value.U)
          edge(); dut.observed.returnPC.expect(value.U)
          pulseCritical()
          // A competing, unaccepted request cannot change the held response owner.
          dut.io.request.bits.instruction.poke(instruction(0x340, write = true).U)
          dut.io.request.bits.source.poke(0xdead.U)
          event(dut.io.request.bits.owner, Event(Owner(15, 9)))
          dut.io.request.valid.poke(true.B)
          for (_ <- 0 until 14) {
            dut.io.request.ready.expect(false.B); dut.io.response.valid.expect(true.B)
            dut.io.response.bits.rob.value.expect(owner.rob.U); dut.io.response.bits.rob.flag.expect(true.B)
            dut.io.response.bits.data.expect(0.U); dut.io.response.bits.flushPipe.expect(true.B)
            dut.observed.returnPC.expect(value.U); dut.observed.returnPCWrite.expect(false.B)
            dut.observed.scratch.expect(scratchSeed.U); dut.observed.debugMode.expect(false.B); edge()
          }
          assert(returnPCWrites == 1 && distributions(0x8b1) == 1 &&
            scratchWrites == scratchBaseline && debugEffects == before)
          dut.io.request.valid.poke(false.B); dut.io.response.ready.poke(true.B); edge()
          dut.io.response.valid.expect(false.B)
          // Pending recovery is not an accepted higher event and cannot discard CSR work.
          write(0x340, 0x7788); dut.observed.scratch.expect(0x7788.U)
          assert(scratchWrites == scratchBaseline + 1 && debugEffects == before)
          begin(instruction(0x340, write = true), 0x99, ready = false)
          edge(); dut.observed.scratch.expect(0x99.U)
          checkpoint("effected-held-response-and-later-csr-complete-once")
          reset(); idle(10)
          dut.io.response.valid.expect(false.B); dut.io.candidate.valid.expect(false.B)
          assert(debugEffects == 0 && returnPCWrites == 0 && scratchWrites == 0)
        }
        runGroup("critical-priority-step-and-single-owner") {
          debugSetup(cetrig = true, step = true)
          val before = debugEffects
          dut.io.halt.poke(true.B); idle(12)
          dut.io.candidate.valid.expect(false.B)
          pulseCritical()
          val selected = Event(Owner(20, 11, 3, flag = true, rvc = true), critical = true)
          claim(selected)
          for (_ <- 0 until 10) {
            dut.io.candidate.valid.expect(false.B); dut.observed.debugMode.expect(false.B); edge()
          }
          assert(debugEffects == before && claims == 1)
          dut.io.halt.poke(false.B)
          delivered(selected, 0x2236, cause = 7)
          assert(debugEffects == before + 1)
          checkpoint("single-critical-owner-completed")
          // Sticky error survives DRET, but a second effect still needs a fresh owner.
          system(BigInt("7b200073", 16))
          dut.observed.debugMode.expect(false.B); dut.observed.critical.expect(true.B)
          idle(10)
          assert(debugEffects == before + 1)
          dut.observed.dpc.expect(0x2236.U)
          val next = Event(Owner(21, 12, 1), critical = true)
          claim(next); idle(3); delivered(next, 0x2246, cause = 7)
          assert(debugEffects == before + 2 && claims == 2)
          checkpoint("sticky-rearm-needs-fresh-owner")

          reset(); debugSetup(cetrig = true)
          val setupEffects = debugEffects
          pulseCritical()
          val abandoned = Event(Owner(22, 13, 2, flag = true), critical = true)
          claim(abandoned)
          for (_ <- 0 until 4) {
            dut.io.candidate.valid.expect(false.B); dut.observed.debugMode.expect(false.B); edge()
          }
          assert(claims == 1 && debugEffects == setupEffects)
          checkpoint("claimed-owner-withheld-before-reset")
          reset(); idle(10)
          dut.io.candidate.valid.expect(false.B); dut.io.response.valid.expect(false.B)
          dut.observed.debugMode.expect(false.B)
          assert(debugEffects == 0 && claims == 0 && huEffects == 0)
          write(0x340, 0x5566); assert(access(0x340) == 0x5566)
          debugSetup(cetrig = true)
          val afterResetSetup = debugEffects
          pulseCritical()
          val afterReset = Event(Owner(23, 14, 3), critical = true)
          claim(afterReset); idle(3); delivered(afterReset, 0x2258, cause = 7)
          assert(debugEffects == afterResetSetup + 1 && claims == 1)
        }
        runGroup("critical-raw-priority-and-older-halt-candidate") {
          debugSetup(cetrig = true)
          val before = debugEffects
          // No older halt exists: both sources participate in this raw selection.
          pulseCritical()
          dut.io.halt.poke(true.B)
          val selected = Event(Owner(21, 10), critical = true)
          claim(selected)
          idle(3)
          assert(debugEffects == before && claims == 1)
          dut.io.halt.poke(false.B)
          delivered(selected, 0x2288, cause = 7)
          checkpoint("simultaneous-raw-critical-wins")

          reset(); debugSetup(cetrig = true)
          val oldHalt = Event(Owner(23, 11, 2), halt = true)
          dut.io.halt.poke(true.B); candidate(oldHalt)
          pulseCritical()
          // The existing pipeline output is still a valid ordinary owner. It may
          // be accepted before the newer critical descriptor traverses the pipe.
          dut.io.candidate.valid.expect(true.B)
          expectDescriptor(dut.io.candidate.bits, oldHalt)
          claim(oldHalt); dut.io.halt.poke(false.B)
          idle(3); delivered(oldHalt, 0x2294, cause = 3)
          dut.observed.critical.expect(true.B)
          checkpoint("older-halt-candidate-retains-owner")
          val afterHalt = debugEffects
          system(BigInt("7b200073", 16))
          dut.observed.debugMode.expect(false.B); idle(10)
          assert(debugEffects == afterHalt)
          val pending = Event(Owner(24, 12, 3), critical = true)
          claim(pending); idle(3); delivered(pending, 0x22a6, cause = 7)
          assert(debugEffects == afterHalt + 1 && claims == 2)
        }
        runGroup("accepted-halt-keeps-identity") {
          debugSetup(cetrig = true)
          val before = debugEffects
          val selected = Event(Owner(22, 12, 2), halt = true)
          dut.io.halt.poke(true.B); claim(selected); dut.io.halt.poke(false.B)
          pulseCritical(); idle(3)
          dut.observed.debugMode.expect(false.B)
          delivered(selected, 0x3344, cause = 3)
          assert(debugEffects == before + 1 && claims == 1)
        }
        runGroup("critical-revokes-unreserved-hu") {
          prepareHU()
          val original = Event(Owner(24, 13, 2), hu = true)
          candidate(original)
          pulseCritical(); idle(2)
          event(dut.io.hu.reserve.bits, original); dut.io.hu.reserve.valid.poke(true.B)
          for (_ <- 0 until 12) {
            dut.io.hu.reserve.ready.expect(false.B); dut.io.hu.completion.valid.expect(false.B)
            dut.observed.entryEffect.expect(false.B); dut.observed.debugMode.expect(false.B); edge()
          }
          dut.io.hu.reserve.valid.poke(false.B)
          assert(reservations == 0 && huEffects == 0)
          val selected = Event(Owner(25, 14, 1), critical = true)
          claim(selected); idle(3); delivered(selected, 0x4456, cause = 7, sourcePrivilege = 0)
        }
        runGroup("reserved-hu-waits-for-owned-pc") {
          prepareHU()
          val original = Event(Owner(26, 15, 3, flag = true, rvc = true), hu = true)
          reserve(original)
          val before = debugEffects
          pulseCritical()
          for (_ <- 0 until 12) {
            dut.observed.debugMode.expect(false.B); dut.observed.entryEffect.expect(false.B)
            dut.io.hu.completion.valid.expect(false.B); dut.io.hu.effectLocked.expect(false.B); edge()
          }
          assert(debugEffects == before && huEffects == 0)
          handoff(original, userPC)
          until("reserved critical Debug target") { bool(dut.observed.debugMode) && bool(dut.io.hu.completion.valid) }
          for (_ <- 0 until 8) {
            checkTerminal(original, outcome = 2, target = debugTarget)
            dut.io.hu.effectLocked.expect(true.B); dut.observed.dpc.expect(userPC.U)
            assert(((uint(dut.observed.dcsr) >> 6) & 7) == 7)
            assert((uint(dut.observed.dcsr) & 3) == 0)
            assert(!uint(dut.observed.dcsr).testBit(5))
            dut.observed.handler.expect(false.B); edge()
          }
          assert(debugEffects == before + 1 && huEffects == 0 && terminals == 0)
          dut.io.hu.completion.ready.poke(true.B); edge()
          dut.io.hu.completion.ready.poke(false.B)
          assert(terminals == 1)
          release(); idle(8)
          assert(debugEffects == before + 1 && terminals == 1)
        }
        runGroup("effected-hu-preserves-target-and-fresh-critical-owner") {
          prepareHU()
          val original = Event(Owner(28, 16, 2), hu = true)
          reserve(original); handoff(original, userPC)
          until("normal HU target")(bool(dut.io.hu.completion.valid))
          for (_ <- 0 until 4) { checkTerminal(original, outcome = 0, target = handlerTarget); edge() }
          assert(huEffects == 0)
          dut.io.hu.completion.ready.poke(true.B); edge()
          dut.io.hu.completion.ready.poke(false.B)
          assert(huEffects == 1 && terminals == 1)
          dut.observed.handler.expect(true.B); dut.observed.uepc.expect(userPC.U)
          val before = debugEffects
          pulseCritical()
          for (_ <- 0 until 12) {
            dut.observed.debugMode.expect(false.B); dut.observed.handler.expect(true.B)
            dut.observed.uepc.expect(userPC.U); dut.io.candidate.valid.expect(false.B)
            dut.io.hu.effectLocked.expect(true.B)
            dut.io.hu.completion.valid.expect(false.B)
            // After the terminal handshake, check the CSR's retained target.
            dut.observed.target.pc.expect(handlerTarget.U)
            dut.observed.target.raiseIPF.expect(false.B)
            dut.observed.target.raiseIAF.expect(false.B)
            dut.observed.target.raiseIGPF.expect(false.B)
            edge()
          }
          assert(debugEffects == before && huEffects == 1 && releases == 0)
          release()
          val fresh = Event(Owner(30, 18, 1, flag = true), critical = true)
          claim(fresh); idle(3); delivered(fresh, handlerTarget + 8, cause = 7, sourcePrivilege = 0)
          dut.observed.uepc.expect(userPC.U); dut.observed.handler.expect(true.B)
          assert(huEffects == 1 && reservations == 1 && terminals == 1 && releases == 1 && claims == 2)
        }
      }
    }
    def finish(error: Option[Throwable]): Unit = {
      val result = Json.obj("status" -> Json.fromString(if (error.isEmpty) "PASS" else "FAIL"),
        "enabled" -> Json.fromBoolean(enabled), "cycles" -> Json.fromInt(cycle),
        "completedGroups" -> Json.arr(completed: _*), "groupCount" -> Json.fromInt(completed.size),
        "error" -> error.map(e => Json.fromString(e.toString)).getOrElse(Json.Null),
        "scope" -> Json.fromString("Real CSR/InterruptFilter/HU public protocols; supplied accepted identities are not ROB retirement or natural watchdog evidence."))
      Files.write(root.resolve("critical-local-result.json"), (result.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
      trace.close()
      if (error.isEmpty) println(s"CRITICAL_LOCAL_PASS enabled=$enabled groups=${completed.size} cycles=$cycle")
    }
  }

  it should "preserve owned effects and native controls" in {
    val root = Paths.get(sys.env("CRITICAL_RUN_ROOT")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated critical evidence directory")
    require(!Files.exists(root.resolve("critical-local-events.jsonl")), "Keep each execution's evidence separate")
    val enabled = sys.env("CRITICAL_FDI_ENABLED") match {
      case "true" | "1" | "on" => true
      case "false" | "0" | "off" => false
      case other => throw new IllegalArgumentException(s"Invalid CRITICAL_FDI_ENABLED: $other")
    }
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled)
    test(new FDICriticalRecoveryLocalHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("compiled-local"))) { dut =>
      val driver = new Driver(dut, enabled, root)
      var error: Option[Throwable] = None
      try driver.run()
      catch { case failure: Throwable => error = Some(failure); throw failure }
      finally driver.finish(error)
    }
  }
}
