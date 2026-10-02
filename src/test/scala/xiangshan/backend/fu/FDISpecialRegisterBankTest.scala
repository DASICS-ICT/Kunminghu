// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.backend.fu.NewCSR.FDISpecialRegisterBank
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.Random

// Only the test shell decodes addresses. Every architectural state bit remains
// owned by the actual production CSRModule instance in this group.
class FDISpecialMapTestHarness(implicit p: Parameters) extends Module with RequireSyncReset {
  private val registers = new FDISpecialRegisterBank
  private val rw = registers.csrRwMap.toSeq
  private val out = registers.csrOutMap.toSeq
  require(registers.csrMods.size == 4 && rw.size == 4)
  require(rw.map(_._1).distinct.size == 4 && out.map(_._1) == rw.map(_._1))
  val io = IO(new Bundle {
    val readAddress = Input(UInt(12.W))
    val readEnable = Input(Bool())
    val readHit = Output(Bool())
    val readData = Output(UInt(64.W))
    val rmwData = Output(UInt(64.W))
    val write = Flipped(Valid(new FDIMainCfgTestWrite))
    val writeCancel = Input(Bool())
    val writeApplied = Output(Bool())
    val addresses = Output(Vec(4, UInt(12.W)))
    val views = Output(Vec(4, UInt(64.W)))
    val rmwViews = Output(Vec(4, UInt(64.W)))
  })
  private val rsel = rw.map { case (address, _) => io.readAddress === address.U }
  private val wsel = rw.map { case (address, _) => io.write.bits.address === address.U }
  io.readHit := VecInit(rsel).asUInt.orR
  io.readData := Mux(io.readEnable, Mux1H(rsel, rw.map(_._2._2)), 0.U)
  io.rmwData := Mux1H(rsel, out.map(_._2))
  io.writeApplied := io.write.valid && VecInit(wsel).asUInt.orR && !io.writeCancel && !reset.asBool
  assert(PopCount(VecInit(wsel)) <= 1.U)
  rw.zip(wsel).foreach { case ((_, (port, _)), selected) =>
    port.wen := io.writeApplied && selected
    port.wdata := io.write.bits.data
  }
  io.addresses := VecInit(rw.map(_._1.U(12.W)))
  io.views := VecInit(rw.map(_._2._2))
  io.rmwViews := VecInit(out.map(_._2))
}

class FDISpecialRegisterBankTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Native DASICS special CSR maps"
  it should "preserve four independent special CSRs and consume saved software writes once" in {
    val root = Paths.get(sys.props.getOrElse("c04.runRoot", throw new IllegalArgumentException("Set c04.runRoot")))
    require(root.isAbsolute && Paths.get("").toRealPath() == root.toRealPath())
    implicit val p: Parameters = Parameters.empty
    // Literal architectural inventory and bit sets form an independent oracle.
    // No implementation address, field, or mask helper is used to predict data.
    val addresses = Vector(0x8b0, 0x8b1, 0x8b2, 0x8b3)
    val bounds = Vector(
      0xbc5, 0xbc6, 0x9e2, 0x9e3, 0x880,
      0x890, 0x891, 0x892, 0x893, 0x894, 0x895, 0x896, 0x897,
      0x898, 0x899, 0x89a, 0x89b, 0x89c, 0x89d, 0x89e, 0x89f,
      0x8a0, 0x8a1, 0x8a2, 0x8a3, 0x8a4, 0x8a5, 0x8a6, 0x8a7,
      0x8a8, 0x8a9, 0x8aa, 0x8ab, 0x8ac, 0x8ad, 0x8ae, 0x8af,
      0x8c0, 0x8c1, 0x8c2, 0x8c3, 0x8c4, 0x8c5, 0x8c6, 0x8c7, 0x8c8)
    val existing = bounds ++ Vector(0xbc4, 0x9e1)
    val combined = existing ++ addresses
    require(combined.distinct.size == 52)
    val u64 = (BigInt(1) << 64) - 1
    val seed = 0xC040026L
    val randomCases = 256
    val categories = mutable.Map.empty[String, Int]
    val events = mutable.Map.empty[String, Int]
    def count(m: mutable.Map[String, Int], k: String): Unit = m(k) = m.getOrElse(k, 0) + 1
    def hex(v: BigInt): String = v.toString(16)
    def bits(a: Int): Set[Int] = a match {
      case 0x8b0 | 0x8b1 | 0x8b2 => (0 to 63).toSet
      case 0x8b3 => Set(0, 1, 2)
      case 0x880 => (0 until 16).flatMap(i => Seq(4*i, 4*i+1, 4*i+3)).toSet
      case 0x8c8 => Set(0, 16, 32, 48)
      case 0xbc4 => (0 to 10).toSet
      case 0x9e1 => Set(1, 6, 7, 8, 9, 10)
      case x if bounds.contains(x) => (3 to 63).toSet
      case _ => Set.empty
    }
    class Model(val order: Vector[Int]) {
      private var state = Map.empty[Int, BigInt].withDefaultValue(BigInt(0))
      def owns(a: Int): Boolean = order.contains(a)
      private def owner(a: Int): Int = if (a == 0x9e1) 0xbc4 else a
      def read(a: Int): BigInt = if (!owns(a)) BigInt(0) else bits(a).foldLeft(BigInt(0)) {
        case (v, b) => if (state(owner(a)).testBit(b)) v.setBit(b) else v
      }
      def write(a: Int, data: BigInt): Unit = {
        require(owns(a))
        val updated = bits(a).foldLeft(state(owner(a))) { case (v, b) =>
          if (data.testBit(b)) v.setBit(b) else v.clearBit(b)
        }
        state = state.updated(owner(a), updated)
      }
      def clear(): Unit = state = Map.empty[Int, BigInt].withDefaultValue(BigInt(0))
      def snapshot: String = order.map(a => hex(read(a))).mkString(":")
    }
    def checkViews(views: Vec[UInt], rmw: Vec[UInt], model: Model): String = {
      model.order.indices.foreach { i =>
        views(i).expect(model.read(model.order(i)).U)
        rmw(i).expect(model.read(model.order(i)).U)
      }
      views.map(x => hex(x.peek().litValue)).mkString(":")
    }
    var directCycles = 0
    var directWrites = 0
    val directTrace = Files.newBufferedWriter(root.resolve("special-trace.csv"), StandardCharsets.UTF_8)
    directTrace.write("cycle,category,address,data,valid,cancel,reset,readAddress,readEnable,readHit,writeApplied,readData,rmwData,before,after\n")
    try {
      test(new FDISpecialMapTestHarness).withAnnotations(Seq(VerilatorBackendAnnotation,
        TargetDirAnnotation("rtl-special-registers"))) { dut =>
        dut.clock.setTimeout(0)
        val model = new Model(addresses)
        addresses.indices.foreach(i => dut.io.addresses(i).expect(addresses(i).U))
        def tick(name: String, address: Int = 0x8b0, data: BigInt = 0, valid: Boolean = false,
          cancel: Boolean = false, reset: Boolean = false, readAddress: Int = 0x8b0,
          readEnable: Boolean = true): Unit = {
          dut.reset.poke(reset.B)
          dut.io.readAddress.poke(readAddress.U)
          dut.io.readEnable.poke(readEnable.B)
          dut.io.write.valid.poke(valid.B)
          dut.io.write.bits.address.poke(address.U)
          dut.io.write.bits.data.poke(data.U(64.W))
          dut.io.writeCancel.poke(cancel.B)
          val hit = model.owns(readAddress)
          val applied = valid && model.owns(address) && !cancel && !reset
          val old = model.snapshot
          withClue(s"direct cycle=$directCycles category=$name address=$address: ") {
            val before = checkViews(dut.io.views, dut.io.rmwViews, model)
            dut.io.readHit.expect(hit.B)
            dut.io.readData.expect((if (readEnable) model.read(readAddress) else BigInt(0)).U)
            dut.io.rmwData.expect(model.read(readAddress).U)
            dut.io.writeApplied.expect(applied.B)
            val observed = Seq(dut.io.readHit.peek().litToBoolean, dut.io.writeApplied.peek().litToBoolean,
              hex(dut.io.readData.peek().litValue), hex(dut.io.rmwData.peek().litValue))
            if (reset) model.clear() else if (applied) model.write(address, data)
            dut.clock.step()
            val after = checkViews(dut.io.views, dut.io.rmwViews, model)
            dut.io.readData.expect((if (readEnable) model.read(readAddress) else BigInt(0)).U)
            dut.io.rmwData.expect(model.read(readAddress).U)
            directTrace.write((Seq(directCycles, name, address.toHexString, hex(data), valid, cancel,
              reset, readAddress.toHexString, readEnable) ++ observed ++ Seq(before, after)).mkString(",")+"\n")
          }
          if (applied) {
            directWrites += 1
            count(events, if (old == model.snapshot) "direct_same_value_write" else "direct_changed_write")
          }
          count(categories, name)
          directCycles += 1
        }
        tick("initial_reset", reset = true)
        for (a <- addresses) tick("reset_read", readAddress = a)
        for (a <- addresses.take(3)) {
          for (value <- Seq(BigInt(0), BigInt(1), BigInt(7), BigInt("8000000000000001", 16),
            BigInt("fffffffffffffff9", 16), u64))
            tick("pc_full_width", a, value, valid = true, readAddress = a)
          for (b <- 0 until 64) {
            tick("pc_walk_one", a, BigInt(1) << b, valid = true, readAddress = a)
            tick("pc_walk_zero", a, u64 ^ (BigInt(1) << b), valid = true, readAddress = a)
          }
        }
        for (value <- 0 to 7; pollution <- Seq(BigInt(0), BigInt(8), BigInt(1) << 31,
          BigInt(1) << 63, u64 ^ BigInt(7))) {
          tick("reason_software_values", 0x8b3, pollution | BigInt(value), valid = true, readAddress = 0x8b3)
          assert(model.read(0x8b3) == value)
        }
        for (b <- 3 until 64)
          tick("reason_high_bit", 0x8b3, (BigInt(1) << b) | BigInt(5), valid = true, readAddress = 0x8b3)
        for (a <- addresses) {
          tick("independent_seed", a, (BigInt(1) << 63) | BigInt(a*19+7), valid = true, readAddress = a)
          tick("same_value", a, model.read(a), valid = true, readAddress = a)
          tick("read_disabled", readAddress = a, readEnable = false)
        }
        // Every unknown address is tested as a write with all state views checked.
        // Existing DASICS groups are intentionally absent from this four-CSR shell.
        for (a <- 0 until 4096)
          tick("address_space", a, u64, valid = !addresses.contains(a), readAddress = a)
        for (a <- addresses; rst <- Seq(false, true); cancel <- Seq(false, true); valid <- Seq(false, true)) {
          tick("control_seed", a, u64, valid = true)
          tick("control_matrix", a, 0, valid, cancel, rst, readAddress = a)
        }
        tick("reason_hold_seed", 0x8b3, 7, valid = true)
        for (_ <- 0 until 8) tick("reason_idle_hold", readAddress = 0x8b3)
        val random = new Random(seed)
        for (_ <- 0 until randomCases) {
          val a = if (random.nextInt(8) == 0) random.nextInt(4096) else addresses(random.nextInt(4))
          tick("direct_random", a, BigInt(64, random), random.nextInt(7) != 0,
            random.nextInt(13) == 0, random.nextInt(127) == 0,
            addresses(random.nextInt(4)), random.nextBoolean())
        }
        tick("final_reset", data = u64, valid = true, reset = true)
      }
    } finally { directTrace.close() }

    case class Request(tag: Int, address: Int, operation: Int = 1, encoding: Int = 1,
      operand: BigInt = 0, rd: Int = 1, readAllowed: Boolean = true, writeAllowed: Boolean = true)
    case class Accepted(request: Request, old: BigInt, finalData: BigInt,
      read: Boolean, write: Boolean, permitted: Boolean)
    var tag = 0
    def request(a: Int, op: Int = 1, enc: Int = 1, data: BigInt = 0, rd: Int = 1,
      ra: Boolean = true, wa: Boolean = true): Request = {
      tag += 1
      Request(tag, a, op, enc, data, rd, ra, wa)
    }
    def instruction(m: Model, q: Request): Accepted = {
      val replace = Set(1, 5).contains(q.operation)
      val read = !replace || q.rd != 0
      val write = replace || q.encoding != 0
      val permit = Set(1, 2, 3, 5, 6, 7).contains(q.operation) && m.owns(q.address) &&
        (!read || q.readAllowed) && (!write || q.writeAllowed)
      val source = if (Set(5, 6, 7).contains(q.operation)) BigInt(q.encoding)
        else if (q.encoding == 0) BigInt(0) else q.operand
      val old = m.read(q.address)
      val result = q.operation match {
        case 1 | 5 => source
        case 2 | 6 => old | source
        case _ => old & (u64 ^ source)
      }
      Accepted(q, if (permit && read) old else BigInt(0), result, permit && read, permit && write, permit)
    }
    var csrCycles = 0
    var accepts = 0
    var responses = 0
    var cancelled = 0
    var csrWrites = 0
    val immediateCases = mutable.Set.empty[(Int, Int, Int)]
    val csrTrace = Files.newBufferedWriter(root.resolve("csr-trace.csv"), StandardCharsets.UTF_8)
    csrTrace.write("cycle,category,phase,reqValid,reqReady,reqTag,reqAddress,reqOperation,reqEncoding,reqOperand,reqRd,readAllowed,writeAllowed,respValid,respReady,pendingTag,pendingAddress,pendingOld,pendingRead,pendingWrite,pendingRejected,savedAddress,savedData,savedPermit,writeApplied,cancel,reset,before,after\n")
    try {
      test(new FDIBoundCSRTestAdapter(includeSpecial = true)).withAnnotations(Seq(VerilatorBackendAnnotation,
        TargetDirAnnotation("rtl-special-combined-csr-adapter"))) { dut =>
        dut.clock.setTimeout(0)
        val model = new Model(combined)
        combined.indices.foreach(i => dut.io.addresses(i).expect(combined(i).U))
        var phase = 0
        var pending = Option.empty[Accepted]
        def tick(name: String, offer: Option[Request] = None, ready: Boolean = true,
          cancel: Boolean = false, reset: Boolean = false): Boolean = {
          val bus = offer.getOrElse(Request(0x70000000+csrCycles, 0xbc4, 7, 31, u64, 0, false, false))
          dut.reset.poke(reset.B)
          dut.io.cancel.poke(cancel.B)
          dut.io.request.valid.poke(offer.nonEmpty.B)
          dut.io.request.bits.tag.poke(bus.tag.U)
          dut.io.request.bits.address.poke(bus.address.U)
          dut.io.request.bits.operation.poke(bus.operation.U)
          dut.io.request.bits.sourceEncoding.poke(bus.encoding.U)
          dut.io.request.bits.operand.poke(bus.operand.U(64.W))
          dut.io.request.bits.destination.poke(bus.rd.U)
          dut.io.request.bits.readAllowed.poke(bus.readAllowed.B)
          dut.io.request.bits.writeAllowed.poke(bus.writeAllowed.B)
          dut.io.response.ready.poke(ready.B)
          val stopped = cancel || reset
          val reqReady = phase == 0 && !stopped
          val respValid = phase != 0 && !stopped
          val fire = reqReady && offer.nonEmpty
          val applied = phase == 1 && !stopped && pending.exists(_.write)
          val oldPhase = phase
          val beforeModel = model.snapshot
          withClue(s"CSR cycle=$csrCycles phase=$phase category=$name bus=$bus pending=$pending: ") {
            val before = checkViews(dut.io.views, dut.io.rmwViews, model)
            dut.io.request.ready.expect(reqReady.B)
            dut.io.response.valid.expect(respValid.B)
            dut.io.writeApplied.expect(applied.B)
            val observedPending: Seq[Any] = if (pending.nonEmpty) {
              val item = pending.get
              dut.io.savedAddress.expect(item.request.address.U)
              dut.io.savedPermit.expect(item.permitted.B)
              if (Set(1,2,3,5,6,7).contains(item.request.operation)) dut.io.savedData.expect(item.finalData.U)
              dut.io.response.bits.tag.expect(item.request.tag.U)
              dut.io.response.bits.address.expect(item.request.address.U)
              dut.io.response.bits.oldData.expect(item.old.U)
              dut.io.response.bits.readPerformed.expect(item.read.B)
              dut.io.response.bits.writeRequested.expect(item.write.B)
              dut.io.response.bits.rejected.expect((!item.permitted).B)
              Seq(dut.io.response.bits.tag.peek().litValue, dut.io.response.bits.address.peek().litValue.toString(16),
                hex(dut.io.response.bits.oldData.peek().litValue), dut.io.response.bits.readPerformed.peek().litToBoolean,
                dut.io.response.bits.writeRequested.peek().litToBoolean, dut.io.response.bits.rejected.peek().litToBoolean,
                dut.io.savedAddress.peek().litValue.toString(16), hex(dut.io.savedData.peek().litValue),
                dut.io.savedPermit.peek().litToBoolean)
            } else Seq.fill(9)("")
            val observed = Seq(dut.io.request.ready.peek().litToBoolean, dut.io.response.valid.peek().litToBoolean,
              dut.io.writeApplied.peek().litToBoolean)
            if (stopped) {
              if (pending.nonEmpty) cancelled += 1
              count(events, s"${if (reset) "reset" else "cancel"}_phase_$phase")
              if (reset) model.clear()
              pending = None
              phase = 0
            } else if (fire) {
              val item = instruction(model, bus)
              dut.io.acceptedOld.expect(item.old.U)
              dut.io.acceptedPermit.expect(item.permitted.B)
              if (Set(1,2,3,5,6,7).contains(bus.operation)) dut.io.acceptedFinal.expect(item.finalData.U)
              pending = Some(item)
              accepts += 1
              phase = 1
              if (!item.permitted) count(events, "rejected")
              else if (!item.write) count(events, "pure_read")
              if (Set(5,6,7).contains(bus.operation)) immediateCases += ((bus.address, bus.operation, bus.encoding))
            } else if (phase == 1) {
              if (applied) {
                model.write(pending.get.request.address, pending.get.finalData)
                csrWrites += 1
                count(events, if (beforeModel == model.snapshot) "csr_same_value_write" else "csr_changed_write")
              }
              if (ready) { pending = None; phase = 0; responses += 1 } else phase = 2
            } else if (respValid && ready) { pending = None; phase = 0; responses += 1 }
            if (respValid && !ready) count(events, "response_hold")
            if (offer.nonEmpty && !reqReady && !stopped) count(events, "request_stall")
            assert(accepts == responses + cancelled + pending.size)
            dut.clock.step()
            val after = checkViews(dut.io.views, dut.io.rmwViews, model)
            csrTrace.write((Seq(csrCycles, name, oldPhase, offer.nonEmpty, observed(0), bus.tag,
              bus.address.toHexString, bus.operation, bus.encoding, hex(bus.operand), bus.rd,
              bus.readAllowed, bus.writeAllowed, observed(1), ready) ++ observedPending ++
              Seq(observed(2), cancel, reset, before, after)).mkString(",")+"\n")
          }
          count(categories, name)
          csrCycles += 1
          fire
        }
        def access(name: String, q: Request, hold: Int = 0): Unit = {
          assert(phase == 0 && pending.isEmpty)
          assert(tick(name+"_accept", Some(q), ready = false))
          tick(name+"_commit_poison", ready = false)
          for (_ <- 0 until hold) tick(name+"_hold", ready = false)
          tick(name+"_response")
        }
        tick("csr_initial_reset", reset = true)
        // Seed every existing owner once, then compare all 52 views after each
        // special write. This checks cross-group isolation without a second
        // exhaustive boundary-register operation matrix.
        for (a <- existing) access("existing_seed", request(a, data = u64 ^ BigInt(a*17)))
        access("main_alias_seed", request(0xbc4, data = 0x7ff))
        access("main_alias_u_clear", request(0x9e1))
        assert(model.read(0xbc4) == 0x3d && model.read(0x9e1) == 0)
        val existingState = existing.map(model.read)
        for (a <- addresses) {
          access("cross_group_special", request(a, data = u64 ^ BigInt(a)))
          assert(existing.map(model.read) == existingState)
        }
        val specialState = addresses.map(model.read)
        for (a <- existing) {
          access("cross_group_existing", request(a, data = BigInt(a*31)))
          assert(addresses.map(model.read) == specialState)
        }
        for (a <- addresses; op <- Seq(1,2,3); enc <- Seq(0,1); value <- Seq(BigInt(0), u64); rd <- Seq(0,1))
          access("register_matrix", request(a, op, enc, value, rd))
        for (a <- addresses; op <- Seq(5,6,7); zimm <- Seq(0,1,7,8,31))
          access("immediate_matrix", request(a, op, zimm, u64, zimm % 2))
        for (a <- addresses; op <- Seq(1,2,3,5,6,7); ra <- Seq(false,true); wa <- Seq(false,true))
          access("permission_matrix", request(a, op, 1, u64, 1, ra, wa))
        for (value <- 0 to 7) {
          access("reason_csr_value", request(0x8b3, data = (u64 ^ BigInt(7)) | BigInt(value)))
          assert(model.read(0x8b3) == value)
          access("reason_pure_read", request(0x8b3, 2, 0, u64, wa = false))
          assert(model.read(0x8b3) == value)
        }
        for (a <- addresses) {
          access("rw_rd_zero", request(a, data = u64, rd = 0, ra = false), hold = 8)
          access("rw_rd_zero_denied", request(a, rd = 0, ra = false, wa = false))
          access("nonzero_encoding_zero_source", request(a, 2, 1, 0))
          access("zero_encoding_poison_source", request(a, 3, 0, u64))
          access("same_value", request(a, data = model.read(a)), hold = 3)
          access("zero_software_write", request(a, data = 0))
          assert(model.read(a) == 0)
        }
        for (a <- Seq(0x8b4,0x8bf,0x88f,0x8c9,0xbc3,0xbc7,0x9e0,0x9e4,0,0xfff);
          op <- Seq(1,2,3,5,6,7)) access("unknown", request(a, op, 1, u64))
        for (op <- Seq(0,4)) access("unknown_operation", request(0x8b3, op, 1, u64))
        for (a <- addresses; rst <- Seq(false,true); cancelPhase <- 0 to 2) {
          access("cancel_seed", request(a, data = u64))
          val committed = if (a == 0x8b3) BigInt(5) else (BigInt(1) << 63) | BigInt(1)
          if (cancelPhase > 0) tick("cancel_accept", Some(request(a, data = committed)))
          if (cancelPhase > 1) tick("cancel_commit", ready = false)
          tick("cancel_matrix", Some(request(0xbc4, data = 0x7ff)), ready = false, cancel = !rst, reset = rst)
          tick("cancel_recovery")
          if (rst) assert(combined.forall(x => model.read(x) == 0))
          else assert(model.read(a) == (if (cancelPhase == 2) committed else if (a == 0x8b3) BigInt(7) else u64))
        }
        // Poisoned live inputs in C1 must neither grant a saved denied write nor
        // revoke an accepted one. The following response stalls cannot reissue it.
        for (a <- addresses) {
          access("saved_denied_seed", request(a, data = u64))
          assert(tick("saved_denied_accept", Some(request(a, data = 0, wa = false)), ready = false))
          tick("saved_denied_live_permit", Some(request(0x8b0, data = 1)), ready = false)
          tick("saved_denied_response")
          assert(model.read(a) == (if (a == 0x8b3) BigInt(7) else u64))
        }
        val stream = (0 until 64).map { i => request(addresses(i % 4), Seq(1,2,3,5,6,7)(i % 6),
          i % 32, (u64-i*73) & u64, i % 32, i % 7 != 0, i % 11 != 0) }
        var index = 0
        while (index < stream.size || phase != 0) {
          val offer = if (index < stream.size) Some(stream(index)) else None
          if (tick("continuous", offer, ready = csrCycles % 5 != 0)) index += 1
        }
        val random = new Random(seed ^ 0xA55AL)
        for (_ <- 0 until randomCases) {
          val q = request(addresses(random.nextInt(4)), Seq(1,2,3,5,6,7)(random.nextInt(6)),
            random.nextInt(32), BigInt(64,random), random.nextInt(32), random.nextInt(11) != 0, random.nextInt(11) != 0)
          assert(tick("random_accept", Some(q), ready = false))
          val cancelBefore = random.nextInt(17) == 0
          tick("random_commit", ready = false, cancel = cancelBefore)
          if (!cancelBefore) {
            for (_ <- 0 until random.nextInt(3)) tick("random_hold", ready = false)
            tick("random_response", cancel = random.nextInt(19) == 0)
          }
          if (random.nextInt(127) == 0) tick("random_reset", reset = true)
        }
        assert(phase == 0 && pending.isEmpty && accepts == responses + cancelled)
        tick("csr_final_reset", reset = true)
      }
    } finally { csrTrace.close() }
    val requiredImmediates = (for (a <- addresses; op <- Seq(5,6,7); zimm <- Seq(0,1,7,8,31)) yield (a,op,zimm)).toSet
    assert(requiredImmediates.subsetOf(immediateCases.toSet))
    for (event <- Seq("direct_same_value_write", "csr_same_value_write", "request_stall", "response_hold",
      "cancel_phase_0", "cancel_phase_1", "cancel_phase_2", "reset_phase_0", "reset_phase_1", "reset_phase_2"))
      assert(events.getOrElse(event, 0) > 0, s"Missing coverage: $event")
    def obj(m: mutable.Map[String,Int]): String = m.toSeq.sortBy(_._1).map { case (k,v) => s"\"$k\":$v" }.mkString("{",",","}")
    val result = s"""{"status":"PASS","seed":$seed,"randomCasesPerPath":$randomCases,
      "c04Addresses":4,"c04Backings":4,"combinedAddresses":52,"combinedBackings":51,
      "directCycles":$directCycles,"directWrites":$directWrites,"csrCycles":$csrCycles,"csrWrites":$csrWrites,
      "accepted":$accepts,"responses":$responses,"cancelled":$cancelled,"remainingInflight":0,
      "c04DirectedImmediateCases":${requiredImmediates.size},"categories":${obj(categories)},"events":${obj(events)},
      "traceValues":"observed DUT reads, effects and pre/post state; independently checked before recording",
      "scope":"Local native CSR groups and test adapters; no production NewCSR, permissions, hardware writers, retirement or physical timing"}
      """
    Files.write(root.resolve("result.json"), result.getBytes(StandardCharsets.UTF_8))
  }
}
