// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.backend.fu.NewCSR.FDIMainCfgBank

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.Random

// This adapter exercises the bank's consumer contract; it is not the production
// CSR wrapper, instruction decoder, permission policy, or retirement machinery.
class FDIMainCfgTestRequest extends Bundle {
  val tag = UInt(32.W)
  val address = UInt(12.W)
  val operation = UInt(3.W)
  val sourceEncoding = UInt(5.W)
  val operand = UInt(64.W)
  val destination = UInt(5.W)
  val readAllowed = Bool()
  val writeAllowed = Bool()
}

class FDIMainCfgTestResponse extends Bundle {
  val tag = UInt(32.W)
  val address = UInt(12.W)
  val oldData = UInt(64.W)
  val readPerformed = Bool()
  val writeRequested = Bool()
  val rejected = Bool()
}

class FDIMainCfgCSRTestAdapter(implicit p: Parameters) extends Module with RequireSyncReset {
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new FDIMainCfgTestRequest))
    val response = Decoupled(new FDIMainCfgTestResponse)
    val cancel = Input(Bool())
    val sView = Output(UInt(64.W))
    val uView = Output(UInt(64.W))
    val writeApplied = Output(Bool())
  })

  val bank = Module(new FDIMainCfgBank)
  val idle :: commit :: respond :: Nil = Enum(3)
  val state = RegInit(idle)
  // Address, data, authority, and response are captured together at acceptance.
  val savedAddress = Reg(UInt(12.W))
  val savedData = Reg(UInt(64.W))
  val savedWrite = Reg(Bool())
  val savedResponse = Reg(new FDIMainCfgTestResponse)
  val request = io.request.bits
  val operationKnown = VecInit(Seq(1, 2, 3, 5, 6, 7).map(request.operation === _.U)).asUInt.orR
  val replace = request.operation === 1.U || request.operation === 5.U
  val wantsRead = !replace || request.destination =/= 0.U
  val wantsWrite = replace || request.sourceEncoding =/= 0.U
  val stopped = reset.asBool || io.cancel
  val permitted = operationKnown && bank.io.readHit &&
    (!wantsRead || request.readAllowed) && (!wantsWrite || request.writeAllowed)
  val source = Mux(request.operation(2), Cat(0.U(59.W), request.sourceEncoding),
    Mux(request.sourceEncoding === 0.U, 0.U(64.W), request.operand))

  io.request.ready := state === idle && !stopped
  bank.io.readAddress := request.address
  bank.io.readEnable := io.request.fire && permitted && wantsRead
  val finalData = Mux(replace, source,
    Mux(request.operation(1, 0) === 2.U, bank.io.readData | source, bank.io.readData & ~source))
  bank.io.write.valid := state === commit && savedWrite && !stopped
  bank.io.write.bits.address := savedAddress
  bank.io.write.bits.data := savedData
  bank.io.writeCancel := io.cancel
  io.sView := bank.io.sView
  io.uView := bank.io.uView
  io.writeApplied := bank.io.writeApplied
  io.response.valid := state =/= idle && !stopped
  io.response.bits := savedResponse

  when (io.cancel) {
    state := idle
  }.otherwise {
    when (io.request.fire) {
      savedAddress := request.address
      savedData := finalData
      savedWrite := permitted && wantsWrite
      savedResponse.tag := request.tag
      savedResponse.address := request.address
      savedResponse.oldData := bank.io.readData
      savedResponse.readPerformed := permitted && wantsRead
      savedResponse.writeRequested := permitted && wantsWrite
      savedResponse.rejected := !permitted
      state := commit
    }.elsewhen (state === commit) {
      // Only this state can issue a write. Holding a response cannot replay it.
      state := Mux(io.response.ready, idle, respond)
    }.elsewhen (io.response.fire) {
      state := idle
    }
  }
}

class FDIMainCfgBankTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDIMainCfgBank"

  it should "share state between both views and consume authorized final writes exactly once" in {
    val runRoot = Paths.get(sys.props.getOrElse("c02.runRoot",
      throw new IllegalArgumentException("Set c02.runRoot to an absolute build output directory")))
    require(runRoot.isAbsolute, "c02.runRoot must be absolute")
    require(Paths.get("").toRealPath() == runRoot.toRealPath(),
      "The test working directory must be c02.runRoot")
    implicit val p: Parameters = Parameters.empty
    val allOnes = (BigInt(1) << 64) - 1
    val seed = 0xC020026L
    val randomCount = 4096
    val categories = mutable.Map.empty[String, Int]
    val events = mutable.Map.empty[String, Int]
    val operationsSeen = mutable.Set.empty[Int]
    val immediateCases = mutable.Set.empty[(Int, Int, Int)]
    def count(target: mutable.Map[String, Int], name: String): Unit =
      target(name) = target.getOrElse(name, 0) + 1
    def hex(value: BigInt): String = value.toString(16)

    // The oracle lists architectural bit positions and updates them individually.
    // It does not use production bundles, constants, masks, or update helpers.
    def visible(address: Int): Set[Int] = address match {
      case 0xBC4 => (0 to 10).toSet
      case 0x9E1 => Set(1, 6, 7, 8, 9, 10)
      case _ => Set.empty
    }
    def pack(bits: Vector[Boolean]): BigInt = bits.zipWithIndex.foldLeft(BigInt(0)) {
      case (value, (set, index)) => if (set) value.setBit(index) else value
    }
    def view(bits: Vector[Boolean], address: Int): BigInt =
      pack(bits.zipWithIndex.map { case (set, index) => set && visible(address).contains(index) })
    def finalWrite(bits: Vector[Boolean], address: Int, data: BigInt): Vector[Boolean] =
      bits.zipWithIndex.map { case (old, index) =>
        if (visible(address).contains(index)) data.testBit(index) else old
      }

    var directCycles = 0
    var directWrites = 0
    val directTrace = Files.newBufferedWriter(runRoot.resolve("bank-trace.csv"), StandardCharsets.UTF_8)
    directTrace.write("cycle,category,address,data,valid,cancel,reset,readAddress,readEnable,readHit,writeApplied,before,after\n")
    try {
      test(new FDIMainCfgBank).withAnnotations(Seq(
        VerilatorBackendAnnotation, TargetDirAnnotation("rtl-maincfg-bank")
      )) { dut =>
        var backing = Vector.fill(11)(false)
        dut.clock.setTimeout(0)
        def tick(name: String, address: Int = 0xBC4, data: BigInt = 0,
          valid: Boolean = false, cancel: Boolean = false, reset: Boolean = false,
          readAddress: Int = 0xBC4, readEnable: Boolean = true): Unit = {
          dut.reset.poke(reset.B)
          dut.io.readAddress.poke(readAddress.U)
          dut.io.readEnable.poke(readEnable.B)
          dut.io.write.valid.poke(valid.B)
          dut.io.write.bits.address.poke(address.U)
          dut.io.write.bits.data.poke(data.U(64.W))
          dut.io.writeCancel.poke(cancel.B)
          val before = pack(backing)
          val hit = visible(readAddress).nonEmpty
          val applied = valid && !cancel && !reset && visible(address).nonEmpty
          withClue(s"direct cycle=$directCycles category=$name address=$address data=$data: ") {
            dut.io.readHit.expect(hit.B)
            dut.io.readData.expect((if (readEnable) view(backing, readAddress) else BigInt(0)).U)
            dut.io.sView.expect(before.U)
            dut.io.uView.expect(view(backing, 0x9E1).U)
            dut.io.writeApplied.expect(applied.B)
            if (reset) backing = Vector.fill(11)(false)
            else if (applied) backing = finalWrite(backing, address, data)
            dut.clock.step()
            dut.io.sView.expect(pack(backing).U)
            dut.io.uView.expect(view(backing, 0x9E1).U)
            dut.io.readData.expect((if (readEnable) view(backing, readAddress) else BigInt(0)).U)
          }
          if (applied) {
            directWrites += 1
            count(events, if (before == pack(backing)) "direct_same_value_write" else "direct_changed_write")
          }
          count(categories, name)
          directTrace.write(Seq(directCycles, name, address.toHexString, hex(data), valid,
            cancel, reset, readAddress.toHexString, readEnable, hit, applied, hex(before), hex(pack(backing))).mkString(",") + "\n")
          directCycles += 1
        }

        tick("direct_initial_reset", reset = true)
        tick("direct_reset_read_s")
        tick("direct_reset_read_u", readAddress = 0x9E1)
        tick("direct_all_ones", data = allOnes, valid = true)
        tick("direct_u_clears_visible", address = 0x9E1, valid = true)
        assert(pack(backing) == 0x03D)
        tick("direct_hidden_read_s")
        tick("direct_hidden_read_u", readAddress = 0x9E1)
        tick("direct_read_disabled", readEnable = false)
        for (address <- Seq(0xBC4, 0x9E1); bit <- 0 until 64) {
          tick("direct_bit_seed", data = allOnes, valid = true)
          tick("direct_bit_replace", address, BigInt(1) << bit, valid = true,
            readAddress = if (address == 0xBC4) 0x9E1 else 0xBC4)
        }
        for (address <- Seq(0xBC3, 0x9E0, 0x000, 0xFFF)) {
          tick("direct_unknown", address, allOnes, valid = true, readAddress = address)
          tick("direct_unknown_disabled", address, allOnes, valid = true,
            readAddress = address, readEnable = false)
        }
        for (reset <- Seq(false, true); cancel <- Seq(false, true); valid <- Seq(false, true)) {
          tick("direct_control_seed", data = allOnes, valid = true)
          tick("direct_control_matrix", address = 0x9E1, data = 0, valid = valid,
            cancel = cancel, reset = reset)
          tick("direct_control_recovery", address = 0x9E1, data = 0x402, valid = true)
        }
        for (index <- 0 until 128) {
          tick("direct_continuous", if (index % 2 == 0) 0xBC4 else 0x9E1,
            if (index % 3 == 0) allOnes else BigInt(index * 37), valid = true,
            readAddress = if (index % 2 == 0) 0x9E1 else 0xBC4)
        }
        val random = new Random(seed)
        for (_ <- 0 until randomCount) {
          val address = if (random.nextBoolean()) 0xBC4 else 0x9E1
          tick("direct_random", address, BigInt(64, random), valid = random.nextInt(9) != 0,
            cancel = random.nextInt(19) == 0, reset = random.nextInt(257) == 0,
            readAddress = if (random.nextBoolean()) 0xBC4 else 0x9E1,
            readEnable = random.nextBoolean())
        }
        tick("direct_final_reset", data = allOnes, valid = true, reset = true)
        tick("direct_final_read")
      }
    } finally { directTrace.close() }

    case class Request(tag: Int, address: Int = 0xBC4, operation: Int = 1,
      sourceEncoding: Int = 1, operand: BigInt = 0, destination: Int = 1,
      readAllowed: Boolean = true, writeAllowed: Boolean = true)
    case class Accepted(request: Request, oldData: BigInt, read: Boolean,
      write: Boolean, rejected: Boolean, after: Vector[Boolean])
    def instruction(bits: Vector[Boolean], request: Request): Accepted = {
      val replace = Set(1, 5).contains(request.operation)
      val reads = !replace || request.destination != 0
      val writes = replace || request.sourceEncoding != 0
      val known = Set(1, 2, 3, 5, 6, 7).contains(request.operation)
      val allowed = known && visible(request.address).nonEmpty &&
        (!reads || request.readAllowed) && (!writes || request.writeAllowed)
      val source = if (Set(5, 6, 7).contains(request.operation)) BigInt(request.sourceEncoding)
        else if (request.sourceEncoding == 0) BigInt(0) else request.operand
      val updated = bits.zipWithIndex.map { case (old, index) =>
        if (!allowed || !writes || !visible(request.address).contains(index)) old
        else request.operation match {
          case 1 | 5 => source.testBit(index)
          case 2 | 6 => old || source.testBit(index)
          case 3 | 7 => old && !source.testBit(index)
        }
      }
      Accepted(request, if (allowed && reads) view(bits, request.address) else BigInt(0),
        allowed && reads, allowed && writes, !allowed, updated)
    }

    var csrCycles = 0
    var acceptedCount = 0
    var responseCount = 0
    var cancelledCount = 0
    var csrWrites = 0
    var nextTag = 0
    def request(address: Int = 0xBC4, operation: Int = 1, sourceEncoding: Int = 1,
      operand: BigInt = 0, destination: Int = 1, readAllowed: Boolean = true,
      writeAllowed: Boolean = true): Request = {
      nextTag += 1
      Request(nextTag, address, operation, sourceEncoding, operand, destination, readAllowed, writeAllowed)
    }
    val csrTrace = Files.newBufferedWriter(runRoot.resolve("csr-trace.csv"), StandardCharsets.UTF_8)
    csrTrace.write("cycle,category,phase,reqValid,reqReady,reqTag,reqAddress,reqOperation,reqEncoding,reqOperand,reqRd,readAllowed,writeAllowed,respValid,respReady,pendingTag,pendingAddress,pendingOld,writeApplied,cancel,reset,before,after\n")
    try {
      test(new FDIMainCfgCSRTestAdapter).withAnnotations(Seq(
        VerilatorBackendAnnotation, TargetDirAnnotation("rtl-maincfg-csr-adapter")
      )) { dut =>
        var backing = Vector.fill(11)(false)
        var phase = 0
        var pending = Option.empty[Accepted]
        dut.clock.setTimeout(0)
        def tick(name: String, offered: Option[Request] = None, responseReady: Boolean = true,
          cancel: Boolean = false, reset: Boolean = false): Boolean = {
          val poison = Request(0x70000000 + csrCycles, address = 0x9E0,
            operation = 7, sourceEncoding = 31, operand = allOnes, destination = 0,
            readAllowed = false, writeAllowed = false)
          val bus = offered.getOrElse(poison)
          dut.reset.poke(reset.B)
          dut.io.cancel.poke(cancel.B)
          dut.io.request.valid.poke(offered.nonEmpty.B)
          dut.io.request.bits.tag.poke(bus.tag.U)
          dut.io.request.bits.address.poke(bus.address.U)
          dut.io.request.bits.operation.poke(bus.operation.U)
          dut.io.request.bits.sourceEncoding.poke(bus.sourceEncoding.U)
          dut.io.request.bits.operand.poke(bus.operand.U(64.W))
          dut.io.request.bits.destination.poke(bus.destination.U)
          dut.io.request.bits.readAllowed.poke(bus.readAllowed.B)
          dut.io.request.bits.writeAllowed.poke(bus.writeAllowed.B)
          dut.io.response.ready.poke(responseReady.B)
          val stopped = cancel || reset
          val ready = phase == 0 && !stopped
          val valid = phase != 0 && !stopped
          val fires = ready && offered.nonEmpty
          val applied = phase == 1 && !stopped && pending.exists(_.write)
          val before = pack(backing)
          val oldPhase = phase
          val oldPending = pending
          withClue(s"CSR cycle=$csrCycles category=$name phase=$phase request=$bus pending=$pending: ") {
            dut.io.request.ready.expect(ready.B)
            dut.io.response.valid.expect(valid.B)
            dut.io.writeApplied.expect(applied.B)
            dut.io.sView.expect(before.U)
            dut.io.uView.expect(view(backing, 0x9E1).U)
            if (valid) {
              val expected = pending.get
              dut.io.response.bits.tag.expect(expected.request.tag.U)
              dut.io.response.bits.address.expect(expected.request.address.U)
              dut.io.response.bits.oldData.expect(expected.oldData.U)
              dut.io.response.bits.readPerformed.expect(expected.read.B)
              dut.io.response.bits.writeRequested.expect(expected.write.B)
              dut.io.response.bits.rejected.expect(expected.rejected.B)
            }
            if (stopped) {
              if (pending.nonEmpty) cancelledCount += 1
              count(events, s"${if (reset) "reset" else "cancel"}_phase_$phase")
              if (reset) backing = Vector.fill(11)(false)
              pending = None
              phase = 0
            } else if (fires) {
              val item = instruction(backing, offered.get)
              pending = Some(item)
              phase = 1
              acceptedCount += 1
              if (item.rejected) count(events, "rejected")
              else if (!item.write) count(events, "pure_read")
              operationsSeen += item.request.operation
              if (Set(5, 6, 7).contains(item.request.operation))
                immediateCases += ((item.request.address, item.request.operation, item.request.sourceEncoding))
            } else if (phase == 1) {
              if (applied) {
                backing = pending.get.after
                csrWrites += 1
                count(events, if (before == pack(backing)) "csr_same_value_write" else "csr_changed_write")
              }
              if (responseReady) {
                pending = None
                phase = 0
                responseCount += 1
              } else phase = 2
            } else if (valid && responseReady) {
              pending = None
              phase = 0
              responseCount += 1
            }
            if (valid && !responseReady) count(events, "response_hold")
            if (offered.nonEmpty && !ready && !stopped) count(events, "request_stall")
            assert(acceptedCount == responseCount + cancelledCount + pending.size)
            dut.clock.step()
            dut.io.sView.expect(pack(backing).U)
            dut.io.uView.expect(view(backing, 0x9E1).U)
            dut.io.request.ready.expect((phase == 0 && !stopped).B)
            dut.io.response.valid.expect((phase != 0 && !stopped).B)
          }
          count(categories, name)
          csrTrace.write(Seq(csrCycles, name, oldPhase, offered.nonEmpty, ready, bus.tag,
            bus.address.toHexString, bus.operation, bus.sourceEncoding, hex(bus.operand), bus.destination,
            bus.readAllowed, bus.writeAllowed, valid, responseReady,
            oldPending.map(_.request.tag.toString).getOrElse(""),
            oldPending.map(_.request.address.toHexString).getOrElse(""),
            oldPending.map(item => hex(item.oldData)).getOrElse(""), applied, cancel, reset,
            hex(before), hex(pack(backing))).mkString(",") + "\n")
          csrCycles += 1
          fires
        }
        def access(name: String, item: Request, hold: Int = 0): Unit = {
          assert(phase == 0 && pending.isEmpty)
          assert(tick(name + "_accept", Some(item), responseReady = false))
          tick(name + "_commit_poison", responseReady = false)
          for (_ <- 0 until hold) tick(name + "_hold", responseReady = false)
          tick(name + "_response")
        }

        tick("csr_initial_reset", reset = true)
        access("csr_seed_7ff", request(operand = 0x7FF))
        access("csr_u_rw_zero", request(address = 0x9E1, operand = 0))
        assert(pack(backing) == 0x03D)
        access("csr_s_read_hidden", request(operation = 2, sourceEncoding = 0, operand = allOnes))
        access("csr_u_read_zero", request(address = 0x9E1, operation = 2, sourceEncoding = 0, operand = allOnes))
        access("csr_rw_rd_zero", request(operand = 0x7FF, destination = 0, readAllowed = false), hold = 8)
        access("csr_rw_rd_zero_write_denied", request(operand = 0, destination = 0,
          readAllowed = false, writeAllowed = false))
        for (address <- Seq(0xBC4, 0x9E1); operation <- Seq(1, 2, 3);
          encoding <- Seq(0, 1, 31); operand <- Seq(BigInt(0), allOnes);
          destination <- Seq(0, 1)) {
          access("csr_register_matrix", request(address, operation, encoding, operand, destination))
        }
        for (address <- Seq(0xBC4, 0x9E1); operation <- Seq(5, 6, 7); zimm <- 0 until 32) {
          access("csr_immediate_matrix", request(address, operation, zimm, allOnes,
            destination = if (zimm % 2 == 0) 0 else 1))
        }
        for (address <- Seq(0xBC4, 0x9E1); operation <- Seq(1, 2, 3, 5, 6, 7);
          readAllowed <- Seq(false, true); writeAllowed <- Seq(false, true)) {
          access("csr_authorization_matrix", request(address, operation, 1, allOnes,
            readAllowed = readAllowed, writeAllowed = writeAllowed))
        }
        for (address <- Seq(0xBC3, 0x9E0, 0x000, 0xFFF); operation <- Seq(1, 2, 3, 5, 6, 7)) {
          access("csr_unknown_address", request(address, operation, 1, allOnes))
        }
        for (operation <- Seq(0, 4)) access("csr_unknown_operation", request(operation = operation, operand = allOnes))

        // Cancel both before and after the single commit edge. A response which
        // is discarded after commit must not roll the architectural state back.
        for (reset <- Seq(false, true); cancelPhase <- 0 to 2; offered <- Seq(false, true)) {
          access("csr_cancel_seed", request(operand = 0x03D))
          if (cancelPhase > 0) tick("csr_cancel_accept", Some(request(address = 0x9E1, operand = 0x7C2)))
          if (cancelPhase > 1) tick("csr_cancel_commit", responseReady = false)
          tick("csr_cancel_matrix", if (offered) Some(request(operand = 0)) else None,
            responseReady = false, cancel = !reset, reset = reset)
          tick("csr_cancel_empty_recovery")
          access("csr_cancel_write_recovery", request(address = 0x9E1, operand = 0))
        }
        tick("csr_both_cancel_accept", Some(request(operand = allOnes)))
        tick("csr_both_cancel", cancel = true, reset = true)
        tick("csr_invalid_poison")

        // Requests remain stable while blocked. The independently captured
        // previous request commits while the next request changes every field.
        val stream = (0 until 128).map { index =>
          request(if (index % 2 == 0) 0xBC4 else 0x9E1,
            Seq(1, 2, 3, 5, 6, 7)(index % 6), index % 32,
            (allOnes - index * 73) & allOnes, index % 32,
            readAllowed = index % 7 != 0, writeAllowed = index % 11 != 0)
        }
        var streamIndex = 0
        while (streamIndex < stream.size || phase != 0) {
          val offered = if (streamIndex < stream.size) Some(stream(streamIndex)) else None
          if (tick("csr_continuous", offered, responseReady = csrCycles % 5 != 0)) streamIndex += 1
        }
        val random = new Random(seed ^ 0xA55AL)
        for (index <- 0 until randomCount) {
          val item = request(if (random.nextBoolean()) 0xBC4 else 0x9E1,
            Seq(1, 2, 3, 5, 6, 7)(random.nextInt(6)), random.nextInt(32),
            BigInt(64, random), random.nextInt(32), readAllowed = random.nextInt(17) != 0,
            writeAllowed = random.nextInt(17) != 0)
          val cancellation = random.nextInt(29) == 0
          val reset = random.nextInt(127) == 0
          val cancelPhase = random.nextInt(3)
          if ((cancellation || reset) && cancelPhase == 0) {
            tick("csr_random_c0_cancel", Some(item), cancel = cancellation, reset = reset)
          } else {
            tick("csr_random_accept", Some(item), responseReady = false)
            if ((cancellation || reset) && cancelPhase == 1) {
              tick("csr_random_c1_cancel", cancel = cancellation, reset = reset)
            } else {
              tick("csr_random_commit_poison", responseReady = false)
              for (_ <- 0 until random.nextInt(3)) tick("csr_random_hold", responseReady = false)
              tick("csr_random_response", cancel = cancellation && cancelPhase == 2,
                reset = reset && cancelPhase == 2)
            }
          }
          count(events, "random_transaction")
        }
        tick("csr_final_reset", Some(request(operand = allOnes)), reset = true)
        tick("csr_final_idle")
        assert(phase == 0 && pending.isEmpty)
      }
    } finally { csrTrace.close() }

    assert(acceptedCount == responseCount + cancelledCount)
    assert(Set(1, 2, 3, 5, 6, 7).subsetOf(operationsSeen))
    for (address <- Seq(0xBC4, 0x9E1); operation <- Seq(5, 6, 7); zimm <- 0 until 32)
      assert(immediateCases.contains((address, operation, zimm)))
    assert(categories("direct_random") == randomCount)
    assert(events("random_transaction") == randomCount)
    for (event <- Seq("direct_same_value_write", "direct_changed_write", "csr_same_value_write",
      "csr_changed_write", "pure_read", "rejected", "response_hold", "request_stall"))
      assert(events.getOrElse(event, 0) > 0, event)
    for (kind <- Seq("reset", "cancel"); phase <- 0 to 2)
      assert(events.getOrElse(s"${kind}_phase_$phase", 0) > 0)
    def mapJson(values: mutable.Map[String, Int]): String = values.toSeq.sortBy(_._1)
      .map { case (name, amount) => s""""$name":$amount""" }.mkString("{", ",", "}")
    val result = s"""{"status":"PASS","scope":"production-bank-and-test-only-csr-adapter","seed":$seed,"directCycles":$directCycles,"directWrites":$directWrites,"csrCycles":$csrCycles,"accepted":$acceptedCount,"responses":$responseCount,"cancelled":$cancelledCount,"csrWrites":$csrWrites,"directRandomCycles":$randomCount,"csrRandomTransactions":$randomCount,"immediateMatrixCases":${immediateCases.size},"events":${mapJson(events)},"categories":${mapJson(categories)}}"""
    Files.write(runRoot.resolve("result.json"), (result + "\n").getBytes(StandardCharsets.UTF_8))
    println(s"C02 MainCfg RTL PASS directCycles=$directCycles directWrites=$directWrites " +
      s"csrCycles=$csrCycles csrWrites=$csrWrites accepted=$acceptedCount responses=$responseCount cancelled=$cancelledCount")
  }
}
