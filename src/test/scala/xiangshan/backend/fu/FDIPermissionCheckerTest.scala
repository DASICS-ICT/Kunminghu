// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.Random

class FDIPermissionCheckerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDIPermissionChecker"

  it should "preserve accepted transactions and cancel them without consuming stale responses" in {
    val runRoot = Paths.get(sys.props.getOrElse("p05.runRoot",
      throw new IllegalArgumentException("Set p05.runRoot to an absolute build output directory")))
    require(runRoot.isAbsolute, "p05.runRoot must be absolute")
    require(Paths.get("").toRealPath() == runRoot.toRealPath(),
      "The test working directory must be p05.runRoot")

    case class Request(pc: BigInt = 0x80001000L, address: BigInt = 0x1008,
      tag: BigInt = 1, size: Int = 0, operation: Int = 0, source: Int = 0,
      virtual: Boolean = false, notTrusted: Boolean = true)
    case class Entry(lo: BigInt = 0x1000, hi: BigInt = 0x1040,
      valid: Boolean = true, read: Boolean = true, write: Boolean = true)
    case class Result(outcome: Int, reason: Int)
    case class Response(request: Request, result: Result)
    val limit = BigInt(1) << 64
    val mask = limit - 1
    val allow = Result(0, 0)
    val readDenied = Result(1, 2)
    val writeDenied = Result(1, 3)
    val guest = Result(2, 0)
    val invalid = Result(3, 0)
    def bank(entries: (Int, Entry)*): Vector[Entry] = entries.foldLeft(Vector.fill(16)(Entry(valid = false))) {
      case (result, (index, entry)) => result.updated(index, entry)
    }
    val emptyBank = bank()
    val fullBank = bank(15 -> Entry())
    def bit(bits: Int, index: Int): Boolean = (bits & (1 << index)) != 0

    // Enumerate every byte and its required permissions within one entry. No DUT
    // arithmetic, operation constants, or policy helper is used by this oracle.
    def rawOracle(request: Request, entries: Vector[Entry]): Boolean = {
      if (request.size > 4 || request.operation > 2) false
      else {
        val required = Vector(Set("read"), Set("write"), Set("read", "write"))(request.operation)
        entries.exists { entry =>
          val permissions = (if (entry.read) Set("read") else Set.empty[String]) ++
            (if (entry.write) Set("write") else Set.empty[String])
          entry.valid && required.subsetOf(permissions) &&
            (0 until (1 << request.size)).forall { offset =>
              val address = request.address + offset
              address < limit && address >= entry.lo && address < entry.hi
            }
        }
      }
    }
    // Columns are disabled/trusted, disabled/untrusted, enabled/trusted,
    // enabled/untrusted; each row explicitly identifies the instruction source.
    val sourceRows = Map(
      (0, false) -> Vector("allow", "allow", "allow", "check"),
      (1, false) -> Vector("allow", "allow", "allow", "check"),
      (2, false) -> Vector("invalid", "invalid", "invalid", "invalid"),
      (3, false) -> Vector("allow", "allow", "allow", "allow"),
      (0, true) -> Vector("allow", "allow", "guest", "guest"),
      (1, true) -> Vector("allow", "allow", "guest", "guest"),
      (2, true) -> Vector("invalid", "invalid", "invalid", "invalid"),
      (3, true) -> Vector("invalid", "invalid", "invalid", "invalid")
    )
    def oracle(request: Request, entries: Vector[Entry], config: Int): Result = {
      if (request.size > 4 || request.operation == 3) invalid
      else {
        val supervisor = request.source == 1
        val enabled = bit(config, if (supervisor) 1 else 0)
        val column = (if (enabled) 2 else 0) + (if (request.notTrusted) 1 else 0)
        sourceRows((request.source, request.virtual))(column) match {
          case "allow" => allow
          case "guest" => guest
          case "invalid" => invalid
          case "check" =>
            val closeIndex = (if (supervisor) 6 else 2) + (if (request.operation == 0) 0 else 1)
            if (bit(config, closeIndex) || rawOracle(request, entries)) allow
            else if (request.operation == 0) readDenied else writeDenied
        }
      }
    }

    val high = (BigInt(1) << 63) + 0x1000
    val explicit = Vector(
      ("read_allow", Request(), fullBank, 3, allow),
      ("read_denied", Request(), emptyBank, 3, readDenied),
      ("write_denied", Request(operation = 1), emptyBank, 3, writeDenied),
      ("rw_no_permission_splice", Request(operation = 2),
        bank(0 -> Entry(write = false), 15 -> Entry(read = false)), 3, writeDenied),
      ("no_range_splice", Request(size = 4),
        bank(0 -> Entry(hi = 0x1010), 15 -> Entry(lo = 0x1010)), 3, readDenied),
      ("rw_close_read_ignored", Request(operation = 2), emptyBank, 7, writeDenied),
      ("rw_close_write", Request(operation = 2), emptyBank, 11, allow),
      ("supervisor_close_read", Request(source = 1), emptyBank, 67, allow),
      ("other_mode_close_ignored", Request(source = 1), emptyBank, 7, readDenied),
      ("trusted", Request(notTrusted = false), emptyBank, 3, allow),
      ("machine", Request(source = 3), emptyBank, 3, allow),
      ("guest_enabled", Request(virtual = true, notTrusted = false), fullBank, 1023, guest),
      ("guest_disabled", Request(virtual = true), emptyBank, 0, allow),
      ("supervisor_guest", Request(source = 1, virtual = true), fullBank, 3, guest),
      ("reserved_source", Request(source = 2, notTrusted = false), fullBank, 0, invalid),
      ("machine_virtual", Request(source = 3, virtual = true), fullBank, 0, invalid),
      ("invalid_operation_machine", Request(source = 3, operation = 3), fullBank, 0, invalid),
      ("invalid_size_disabled", Request(size = 7, notTrusted = false), fullBank, 0, invalid),
      ("unsigned_high_address", Request(address = high + 8, size = 3),
        bank(7 -> Entry(lo = high, hi = high + 64)), 3, allow),
      ("high_address_no_alias", Request(address = high + 8, size = 3), fullBank, 3, readDenied),
      ("exact_low_bits", Request(address = 0x1003),
        bank(2 -> Entry(lo = 0x1003, hi = 0x1004)), 3, allow),
      ("exclusive_end", Request(address = 0x1040), fullBank, 3, readDenied),
      ("overflow", Request(address = mask - 1, size = 2),
        bank(3 -> Entry(lo = mask - 32, hi = mask)), 3, readDenied),
      ("empty_bound", Request(), bank(4 -> Entry(lo = 0x1008, hi = 0x1008)), 3, readDenied),
      ("reverse_bound", Request(), bank(4 -> Entry(lo = 0x1010, hi = 0x1000)), 3, readDenied)
    )
    explicit.foreach { case (name, request, entries, config, expected) =>
      assert(oracle(request, entries, config) == expected, s"$name independent expectation")
    }

    val seed = 0x503035L
    val randomCycles = 8192
    var cycles = 0
    var accepted = 0
    var consumed = 0
    var cancelled = 0
    val outcomes = Array.fill(4)(0)
    val sources = Array.fill(8)(0)
    val sizes = Array.fill(8)(0)
    val operations = Array.fill(4)(0)
    val categories = mutable.Map.empty[String, Int]
    val events = mutable.Map.empty[String, Int]
    val cancelMatrix = mutable.Set.empty[String]
    def count(target: mutable.Map[String, Int], name: String): Unit =
      target(name) = target.getOrElse(name, 0) + 1
    val queue = mutable.Queue.empty[Response]
    val trace = Files.newBufferedWriter(runRoot.resolve("transaction-trace.csv"), StandardCharsets.UTF_8)
    trace.write("cycle,category,reset,flush,reqValid,reqReady,respValid,respReady,reqFire,respFire,requestTag,responseTag,acceptedOutcome,pendingAfter,accepted,consumed,cancelled\n")

    try {
      test(new FDIPermissionChecker).withAnnotations(Seq(
        VerilatorBackendAnnotation, TargetDirAnnotation("rtl-permission-transaction")
      )) { dut =>
        var stalledRequest: Option[Request] = None
        def pokeRequest(port: FDIPermissionRequest, request: Request): Unit = {
          port.pc.poke(request.pc.U(64.W))
          port.address.poke(request.address.U(64.W))
          port.tag.poke(request.tag.U(64.W))
          port.sizeLog2.poke(request.size.U(3.W))
          port.operation.poke(request.operation.U(2.W))
          port.sourcePrivilege.poke(request.source.U(2.W))
          port.sourceVirtual.poke(request.virtual.B)
          port.notTrusted.poke(request.notTrusted.B)
        }
        def expectResponse(response: Response): Unit = {
          val request = response.request
          val port = dut.io.resp.bits.request
          port.pc.expect(request.pc.U(64.W))
          port.address.expect(request.address.U(64.W))
          port.tag.expect(request.tag.U(64.W))
          port.sizeLog2.expect(request.size.U(3.W))
          port.operation.expect(request.operation.U(2.W))
          port.sourcePrivilege.expect(request.source.U(2.W))
          port.sourceVirtual.expect(request.virtual.B)
          port.notTrusted.expect(request.notTrusted.B)
          dut.io.resp.bits.outcome.expect(response.result.outcome.U(2.W))
          dut.io.resp.bits.reason.expect(response.result.reason.U(3.W))
        }
        def tick(name: String, request: Option[Request], responseReady: Boolean,
          entries: Vector[Entry] = fullBank, config: Int = 3,
          flush: Boolean = false, reset: Boolean = false): Boolean = {
          val cancel = flush || reset
          if (!cancel) stalledRequest.foreach { previous =>
            assert(request.contains(previous), s"cycle=$cycles driver changed a stalled request")
          }
          val poison = Request(pc = mask - cycles, address = mask, tag = mask - cycles,
            size = 7, operation = 3, source = 2, virtual = true, notTrusted = false)
          dut.reset.poke(reset.B)
          dut.io.flush.poke(flush.B)
          dut.io.req.valid.poke(request.nonEmpty.B)
          pokeRequest(dut.io.req.bits, request.getOrElse(poison))
          dut.io.resp.ready.poke(responseReady.B)
          val configPorts = Seq(dut.io.config.uEnable, dut.io.config.sEnable,
            dut.io.config.uCloseRead, dut.io.config.uCloseWrite, dut.io.config.uCloseJump,
            dut.io.config.uCloseEcall, dut.io.config.sCloseRead, dut.io.config.sCloseWrite,
            dut.io.config.sCloseJump, dut.io.config.sCloseEcall)
          configPorts.zipWithIndex.foreach { case (port, index) => port.poke(bit(config, index).B) }
          entries.zipWithIndex.foreach { case (entry, index) =>
            val port = dut.io.entries(index)
            port.boundLo.poke(entry.lo.U(64.W))
            port.boundHi.poke(entry.hi.U(64.W))
            port.entryValid.poke(entry.valid.B)
            port.readAllowed.poke(entry.read.B)
            port.writeAllowed.poke(entry.write.B)
          }
          val ready = !cancel && (queue.isEmpty || responseReady)
          val valid = !cancel && queue.nonEmpty
          val requestFire = ready && request.nonEmpty
          val responseFire = valid && responseReady
          val oldResponse = queue.headOption
          val incoming = if (requestFire) request.map(r => Response(r, oracle(r, entries, config))) else None
          val clue = s"cycle=$cycles category=$name reset=$reset flush=$flush request=$request " +
            s"responseReady=$responseReady config=$config entries=$entries queue=$queue: "
          withClue(clue) {
            dut.io.req.ready.expect(ready.B)
            dut.io.resp.valid.expect(valid.B)
            if (valid) expectResponse(queue.front)
            if (cancel) {
              cancelMatrix += s"${if (reset) "reset" else "flush"}_${queue.nonEmpty}_${request.nonEmpty}_$responseReady"
              count(events, if (queue.nonEmpty) "cancel_full" else "cancel_empty")
              cancelled += queue.size
              queue.clear()
            } else {
              if (valid && !responseReady) count(events, "response_hold")
              if (request.nonEmpty && !ready) count(events, "request_stall")
              if (queue.isEmpty && requestFire && !responseReady) count(events, "empty_accept_backpressure")
              if (requestFire && responseFire) count(events, "replacement")
              if (!requestFire && responseFire) count(events, "consume_only")
              if (queue.isEmpty && !requestFire) count(events, "empty_bubble")
              if (responseFire) {
                queue.dequeue()
                consumed += 1
              }
              incoming.foreach { response =>
                queue.enqueue(response)
                accepted += 1
                outcomes(response.result.outcome) += 1
                sources(response.request.source * 2 + (if (response.request.virtual) 1 else 0)) += 1
                sizes(response.request.size) += 1
                operations(response.request.operation) += 1
              }
            }
            assert(queue.size <= 1)
            assert(accepted == consumed + cancelled + queue.size)
            dut.clock.step()
            // Evaluate the new state with the same external levels, without a
            // second clock edge. Empty acceptance therefore cannot fall through.
            dut.io.req.ready.expect((!cancel && (queue.isEmpty || responseReady)).B)
            dut.io.resp.valid.expect((!cancel && queue.nonEmpty).B)
            if (!cancel && queue.nonEmpty) expectResponse(queue.front)
          }
          stalledRequest = if (!cancel && request.nonEmpty && !ready) request else None
          count(categories, name)
          trace.write(Seq(cycles, name, reset, flush, request.nonEmpty, ready, valid,
            responseReady, requestFire, responseFire,
            request.map(_.tag.toString).getOrElse(""),
            oldResponse.map(_.request.tag.toString).getOrElse(""),
            incoming.map(_.result.outcome.toString).getOrElse(""), queue.size,
            accepted, consumed, cancelled).mkString(",") + "\n")
          cycles += 1
          requestFire
        }

        dut.clock.setTimeout(0)
        tick("initial_reset", None, responseReady = false, reset = true)
        tick("initial_empty", None, responseReady = true)
        explicit.zipWithIndex.foreach { case ((name, request, entries, config, _), index) =>
          tick(name, Some(request.copy(tag = index + 1)), responseReady = false, entries = entries, config = config)
          tick("explicit_consume", None, responseReady = true, entries = emptyBank, config = 0)
        }

        // All representable malformed descriptors are rejected even in modes
        // which otherwise bypass permission checking.
        for (size <- 0 until 8; operation <- 0 until 4 if size > 4 || operation == 3;
          source <- Seq(0, 3); config <- Seq(0, 1023)) {
          val request = Request(tag = 0, size = size, operation = operation,
            source = source, notTrusted = false)
          assert(oracle(request, fullBank, config) == invalid)
          tick("invalid_descriptor", Some(request), responseReady = false, entries = fullBank, config = config)
          tick("invalid_consume", None, responseReady = true)
        }

        // Each replacement changes metadata and alternates a granted/denied
        // outcome. Repeated tags must not cause association by tag alone.
        for (index <- 0 until 96) {
          val address = high + index * 32
          val request = Request(pc = mask - index * 4, address = address,
            tag = index % 3, size = index % 5, operation = index % 3,
            source = index % 2)
          val entries = if (index % 2 == 0)
            bank(index % 16 -> Entry(lo = address, hi = address + 16)) else emptyBank
          val expected = if (index % 2 == 0) allow
            else if (request.operation == 0) readDenied else writeDenied
          assert(oracle(request, entries, 3) == expected)
          tick("continuous_replacement", Some(request), responseReady = true, entries = entries)
        }
        tick("stream_drain", None, responseReady = true)

        val blocked = Request(pc = mask, tag = mask, size = 4, operation = 2, source = 1)
        tick("hold_seed", Some(Request(tag = 81)), responseReady = false)
        for (index <- 0 until 24) {
          tick("pre_accept_config_change", Some(blocked), responseReady = false,
            entries = if (index % 2 == 0) emptyBank else fullBank, config = if (index % 3 == 0) 0 else 1023)
        }
        tick("accept_final_config", Some(blocked), responseReady = true, entries = emptyBank, config = 3)
        for (_ <- 0 until 16) {
          tick("post_accept_config_change", None, responseReady = false, entries = fullBank, config = 1023)
        }
        tick("held_response_drain", None, responseReady = true)
        for (_ <- 0 until 8) tick("poison_bubble", None, responseReady = false, entries = emptyBank, config = 0)

        // Cover cancellation with empty/full state and every interface intent.
        // Only the accepted slot is counted as cancelled, never a blocked offer.
        for (reset <- Seq(false, true); full <- Seq(false, true);
          requestValid <- Seq(false, true); responseReady <- Seq(false, true)) {
          if (full) tick("cancel_seed", Some(Request(tag = 91)), responseReady = false)
          tick("cancel_matrix", if (requestValid) Some(Request(tag = 92)) else None,
            responseReady, flush = !reset, reset = reset)
          tick("cancel_recovery", None, responseReady = true)
        }
        tick("both_cancel_seed", Some(Request(tag = 93)), responseReady = false)
        tick("both_cancel", Some(Request(tag = 94)), responseReady = true, flush = true, reset = true)
        tick("after_both_cancel", Some(Request(tag = 95)), responseReady = false)
        tick("after_both_drain", None, responseReady = true)

        val random = new Random(seed)
        var pending: Option[Request] = None
        for (_ <- 0 until randomCycles) {
          val reset = random.nextInt(101) == 0
          val flush = random.nextInt(43) == 0
          if (pending.isEmpty && random.nextInt(5) != 0) {
            pending = Some(Request(pc = BigInt(64, random), address = BigInt(64, random),
              tag = if (random.nextBoolean()) BigInt(random.nextInt(4)) else BigInt(64, random),
              size = random.nextInt(8), operation = random.nextInt(4), source = random.nextInt(4),
              virtual = random.nextBoolean(), notTrusted = random.nextBoolean()))
          }
          val entries = Vector.fill(16)(Entry(lo = BigInt(64, random), hi = BigInt(64, random),
            valid = random.nextBoolean(), read = random.nextBoolean(), write = random.nextBoolean()))
          val matching = pending.filter(_.address <= mask - 16).map { request =>
            entries.updated(random.nextInt(16), Entry(lo = request.address, hi = request.address + 16,
              read = random.nextBoolean(), write = random.nextBoolean()))
          }.getOrElse(entries)
          val fired = tick("random", pending, random.nextInt(4) != 0,
            matching, random.nextInt(1024), flush, reset)
          if (fired || flush || reset) pending = None
        }
        pending.foreach { request => tick("random_pending_drain", Some(request), responseReady = true) }
        tick("final_drain", None, responseReady = true)
        tick("final_empty", None, responseReady = true)
      }
    } finally {
      trace.close()
    }

    assert(queue.isEmpty && accepted == consumed + cancelled)
    assert(outcomes.forall(_ > 0) && sources.forall(_ > 0))
    assert(sizes.forall(_ > 0) && operations.forall(_ > 0))
    assert(categories("random") == randomCycles)
    for (kind <- Seq("flush", "reset"); full <- Seq(false, true);
      requestValid <- Seq(false, true); responseReady <- Seq(false, true)) {
      assert(cancelMatrix.contains(s"${kind}_${full}_${requestValid}_$responseReady"))
    }
    for (event <- Seq("cancel_full", "cancel_empty", "response_hold", "request_stall",
      "empty_accept_backpressure", "replacement", "consume_only", "empty_bubble")) {
      assert(events.getOrElse(event, 0) > 0, event)
    }
    def mapJson(values: mutable.Map[String, Int]): String = values.toSeq.sortBy(_._1)
      .map { case (name, count) => s""""$name":$count""" }.mkString("{", ",", "}")
    val result = s"""{"status":"PASS","seed":$seed,"cycles":$cycles,"randomCycles":$randomCycles,"accepted":$accepted,"consumed":$consumed,"cancelled":$cancelled,"inFlight":${queue.size},"outcomes":${outcomes.mkString("[", ",", "]")},"sources":${sources.mkString("[", ",", "]")},"sizes":${sizes.mkString("[", ",", "]")},"operations":${operations.mkString("[", ",", "]")},"cancelMatrixCases":${cancelMatrix.size},"events":${mapJson(events)},"categories":${mapJson(categories)}}"""
    Files.write(runRoot.resolve("result.json"), (result + "\n").getBytes(StandardCharsets.UTF_8))
    println(s"P05 transaction RTL PASS cycles=$cycles accepted=$accepted consumed=$consumed cancelled=$cancelled")
  }
}
