// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.backend.fu.NewCSR.{FDIBoundRegisterBank, FDIMainCfgBank}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.Random

// Test-only dispatch over the actual native CSR instance groups. No CSR state
// is reimplemented here. C05 will own production dispatch and authorization.
class FDIBoundMapTestHarness(includeMainCfg: Boolean)(implicit p: Parameters)
  extends Module with RequireSyncReset {
  private val bounds = new FDIBoundRegisterBank
  private val main = if (includeMainCfg) Some(new FDIMainCfgBank) else None
  private val mainRw = main.toSeq.flatMap(_.csrRwMap.toSeq)
  private val mainOut = main.toSeq.flatMap(_.csrOutMap.toSeq)
  private val rw = bounds.csrRwMap.toSeq ++ mainRw
  private val out = bounds.csrOutMap.toSeq ++ mainOut
  require(rw.size == (if (includeMainCfg) 48 else 46))
  require(rw.map(_._1).distinct.size == rw.size)
  require(out.map(_._1) == rw.map(_._1))
  require(bounds.csrMods.size + main.toSeq.flatMap(_.csrMods).size == (if (includeMainCfg) 47 else 46))
  val io = IO(new Bundle {
    val readAddress = Input(UInt(12.W))
    val readEnable = Input(Bool())
    val readHit = Output(Bool())
    val readData = Output(UInt(64.W))
    val rmwData = Output(UInt(64.W))
    val write = Flipped(Valid(new FDIMainCfgTestWrite))
    val writeCancel = Input(Bool())
    val writeApplied = Output(Bool())
    val addresses = Output(Vec(rw.size, UInt(12.W)))
    val views = Output(Vec(rw.size, UInt(64.W)))
    val rmwViews = Output(Vec(out.size, UInt(64.W)))
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

// The single-slot adapter consumes a saved final-write transaction exactly
// once. Its policy inputs are local test controls, not production permissions.
class FDIBoundCSRTestAdapter(implicit p: Parameters) extends Module with RequireSyncReset {
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new FDIMainCfgTestRequest))
    val response = Decoupled(new FDIMainCfgTestResponse)
    val cancel = Input(Bool())
    val views = Output(Vec(48, UInt(64.W)))
    val rmwViews = Output(Vec(48, UInt(64.W)))
    val writeApplied = Output(Bool())
    val savedAddress = Output(UInt(12.W))
    val savedData = Output(UInt(64.W))
    val savedPermit = Output(Bool())
    val acceptedOld = Output(UInt(64.W))
    val acceptedFinal = Output(UInt(64.W))
    val acceptedPermit = Output(Bool())
  })
  private val bank = Module(new FDIBoundMapTestHarness(true))
  private val idle :: commit :: respond :: Nil = Enum(3)
  private val state = RegInit(idle)
  private val savedAddress = Reg(UInt(12.W))
  private val savedData = Reg(UInt(64.W))
  private val savedWrite = Reg(Bool())
  private val savedPermit = Reg(Bool())
  private val savedResponse = Reg(new FDIMainCfgTestResponse)
  private val request = io.request.bits
  private val known = VecInit(Seq(1, 2, 3, 5, 6, 7).map(request.operation === _.U)).asUInt.orR
  private val replace = request.operation === 1.U || request.operation === 5.U
  private val wantsRead = !replace || request.destination =/= 0.U
  private val wantsWrite = replace || request.sourceEncoding =/= 0.U
  private val stopped = reset.asBool || io.cancel
  private val permitted = known && bank.io.readHit &&
    (!wantsRead || request.readAllowed) && (!wantsWrite || request.writeAllowed)
  private val source = Mux(request.operation(2), Cat(0.U(59.W), request.sourceEncoding),
    Mux(request.sourceEncoding === 0.U, 0.U(64.W), request.operand))
  io.request.ready := state === idle && !stopped
  bank.io.readAddress := request.address
  bank.io.readEnable := io.request.fire && permitted && wantsRead
  private val finalData = Mux(replace, source,
    Mux(request.operation(1, 0) === 2.U, bank.io.rmwData | source, bank.io.rmwData & ~source))
  bank.io.write.valid := state === commit && savedWrite && !stopped
  bank.io.write.bits.address := savedAddress
  bank.io.write.bits.data := savedData
  bank.io.writeCancel := io.cancel
  io.views := bank.io.views
  io.rmwViews := bank.io.rmwViews
  io.writeApplied := bank.io.writeApplied
  io.savedAddress := savedAddress
  io.savedData := savedData
  io.savedPermit := savedPermit
  io.acceptedOld := bank.io.readData
  io.acceptedFinal := finalData
  io.acceptedPermit := permitted
  io.response.valid := state =/= idle && !stopped
  io.response.bits := savedResponse
  when(io.cancel) { state := idle }.otherwise {
    when(io.request.fire) {
      savedAddress := request.address
      savedData := finalData
      savedWrite := permitted && wantsWrite
      savedPermit := permitted
      savedResponse.tag := request.tag
      savedResponse.address := request.address
      savedResponse.oldData := bank.io.readData
      savedResponse.readPerformed := permitted && wantsRead
      savedResponse.writeRequested := permitted && wantsWrite
      savedResponse.rejected := !permitted
      state := commit
    }.elsewhen(state === commit) {
      state := Mux(io.response.ready, idle, respond)
    }.elsewhen(io.response.fire) { state := idle }
  }
}

class FDIBoundRegisterBankTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Native DASICS CSR maps"
  it should "preserve all C03 state and consume C02/C03 final writes without cross-address effects" in {
    val root = Paths.get(sys.props.getOrElse("c03.runRoot", throw new IllegalArgumentException("Set c03.runRoot")))
    require(root.isAbsolute && Paths.get("").toRealPath() == root.toRealPath())
    implicit val p: Parameters = Parameters.empty
    // Independent literal inventory. Never import the implementation's address
    // object, bundles, masks, or functions into the oracle.
    val addresses = Vector(
      0xbc5, 0xbc6, 0x9e2, 0x9e3, 0x880,
      0x890, 0x891, 0x892, 0x893, 0x894, 0x895, 0x896, 0x897,
      0x898, 0x899, 0x89a, 0x89b, 0x89c, 0x89d, 0x89e, 0x89f,
      0x8a0, 0x8a1, 0x8a2, 0x8a3, 0x8a4, 0x8a5, 0x8a6, 0x8a7,
      0x8a8, 0x8a9, 0x8aa, 0x8ab, 0x8ac, 0x8ad, 0x8ae, 0x8af,
      0x8c0, 0x8c1, 0x8c2, 0x8c3, 0x8c4, 0x8c5, 0x8c6, 0x8c7, 0x8c8)
    val combined = addresses ++ Vector(0xbc4, 0x9e1)
    val u64 = (BigInt(1) << 64) - 1
    val seed = 0xC030026L
    val randomCases = 4096
    val categories = mutable.Map.empty[String, Int]
    val events = mutable.Map.empty[String, Int]
    def count(m: mutable.Map[String, Int], k: String): Unit = m(k) = m.getOrElse(k, 0) + 1
    def hex(v: BigInt): String = v.toString(16)
    def bits(a: Int): Set[Int] = a match {
      case 0x880 => (0 until 16).flatMap(i => Seq(4*i, 4*i+1, 4*i+3)).toSet
      case 0x8c8 => Set(0, 16, 32, 48)
      case 0xbc4 => (0 to 10).toSet
      case 0x9e1 => Set(1, 6, 7, 8, 9, 10)
      case x if addresses.contains(x) => (3 to 63).toSet
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
    val directTrace = Files.newBufferedWriter(root.resolve("bound-trace.csv"), StandardCharsets.UTF_8)
    directTrace.write("cycle,category,address,data,valid,cancel,reset,readAddress,readEnable,readHit,writeApplied,readData,rmwData,before,after\n")
    try {
      test(new FDIBoundMapTestHarness(false)).withAnnotations(Seq(VerilatorBackendAnnotation,
        TargetDirAnnotation("rtl-bound-registers"))) { dut =>
        dut.clock.setTimeout(0)
        val model = new Model(addresses)
        addresses.indices.foreach(i => dut.io.addresses(i).expect(addresses(i).U))
        def tick(name: String, address: Int = 0xbc5, data: BigInt = 0, valid: Boolean = false,
          cancel: Boolean = false, reset: Boolean = false, readAddress: Int = 0xbc5,
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
        for (a <- addresses) {
          tick("all_zero", a, 0, valid = true, readAddress = a)
          tick("all_one", a, u64, valid = true, readAddress = a)
          tick("same_value", a, u64, valid = true, readAddress = a)
          for (b <- 0 until 64) {
            tick("walk_one", a, BigInt(1) << b, valid = true, readAddress = a)
            tick("walk_zero", a, u64 ^ (BigInt(1) << b), valid = true, readAddress = a)
          }
        }
        for (slot <- 0 until 16) {
          tick("lib_slot_f", 0x880, BigInt(15) << (4*slot), valid = true, readAddress = 0x880)
          assert(model.read(0x880) == (BigInt(11) << (4*slot)))
          tick("lib_slot_4", 0x880, BigInt(4) << (4*slot), valid = true, readAddress = 0x880)
          assert(model.read(0x880) == 0)
          tick("lib_slot_seed", 0x880, u64, valid = true)
          tick("lib_slot_preserve", 0x880, u64 ^ (BigInt(15) << (4*slot)), valid = true)
        }
        val boundAddresses = addresses.filter(a => a != 0x880 && a != 0x8c8)
        for (pair <- boundAddresses.grouped(2)) {
          tick("range_cfg_seed_lib", 0x880, u64, valid = true)
          tick("range_cfg_seed_jump", 0x8c8, u64, valid = true)
          for ((lo, hi) <- Seq((BigInt(0), BigInt(0)),
            (BigInt("8000000000000040", 16), BigInt("8000000000000040", 16)),
            (BigInt("fffffffffffffff8", 16), BigInt(8)), (BigInt(8), BigInt(0)))) {
            tick("range_lo", pair(0), lo, valid = true, readAddress = pair(1))
            tick("range_hi", pair(1), hi, valid = true, readAddress = pair(0))
            assert(model.read(0x880) == BigInt("bbbbbbbbbbbbbbbb", 16))
            assert(model.read(0x8c8) == BigInt("0001000100010001", 16))
          }
        }
        // Exhaust the 12-bit address space. Every non-member is also attempted
        // as a write, including C02, C04, legacy aliases and address holes.
        for (a <- 0 until 4096) {
          tick("address_space", a, u64, valid = !addresses.contains(a), readAddress = a)
          tick("read_disabled", a, 0, valid = false, readAddress = a, readEnable = false)
        }
        for ((w, wi) <- addresses.zipWithIndex; (r, ri) <- addresses.zipWithIndex) {
          tick("cross_address", w, (BigInt(1) << 63) | BigInt(wi*65537+ri*31+7), valid = true, readAddress = r)
        }
        for (a <- addresses; rst <- Seq(false, true); cancel <- Seq(false, true); valid <- Seq(false, true)) {
          tick("control_seed", a, u64, valid = true)
          tick("control_matrix", a, 0, valid, cancel, rst, readAddress = a)
          tick("control_recovery", a, u64, valid = true, readAddress = a)
        }
        val random = new Random(seed)
        for (_ <- 0 until randomCases) {
          val a = if (random.nextInt(10) == 0) random.nextInt(4096) else addresses(random.nextInt(46))
          tick("direct_random", a, BigInt(64, random), random.nextInt(9) != 0,
            random.nextInt(19) == 0, random.nextInt(257) == 0,
            addresses(random.nextInt(46)), random.nextBoolean())
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
      test(new FDIBoundCSRTestAdapter).withAnnotations(Seq(VerilatorBackendAnnotation,
        TargetDirAnnotation("rtl-combined-csr-adapter"))) { dut =>
        dut.clock.setTimeout(0)
        val model = new Model(combined)
        var phase = 0
        var pending = Option.empty[Accepted]
        def tick(name: String, offer: Option[Request] = None, ready: Boolean = true,
          cancel: Boolean = false, reset: Boolean = false): Boolean = {
          val bus = offer.getOrElse(Request(0x70000000+csrCycles, 0x8b3, 7, 31, u64, 0, false, false))
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
        access("main_seed_7ff", request(0xbc4, data = 0x7ff))
        access("main_u_zero", request(0x9e1))
        assert(model.read(0xbc4) == 0x3d && model.read(0x9e1) == 0)
        for (a <- addresses; op <- Seq(1,2,3); enc <- Seq(0,1,31); value <- Seq(BigInt(0), u64); rd <- Seq(0,1))
          access("register_matrix", request(a, op, enc, value, rd))
        for (a <- addresses; op <- Seq(5,6,7); zimm <- 0 until 32)
          access("immediate_matrix", request(a, op, zimm, u64, zimm % 2))
        for (a <- combined; op <- Seq(1,2,3,5,6,7); ra <- Seq(false,true); wa <- Seq(false,true))
          access("permission_matrix", request(a, op, 1, u64, 1, ra, wa))
        for (slot <- 0 until 16) {
          access("slot_rw_f", request(0x880, data = BigInt(15) << (4*slot)))
          assert(model.read(0x880) == (BigInt(11) << (4*slot)))
          access("slot_rw_4", request(0x880, data = BigInt(4) << (4*slot)))
          assert(model.read(0x880) == 0)
          access("slot_seed", request(0x880, data = u64))
          access("slot_clear", request(0x880, 3, 1, BigInt(15) << (4*slot)))
          access("slot_set", request(0x880, 2, 1, BigInt(15) << (4*slot)))
        }
        for (a <- combined) {
          access("rw_rd_zero", request(a, data = u64, rd = 0, ra = false), hold = 8)
          access("rw_rd_zero_denied", request(a, rd = 0, ra = false, wa = false))
          access("nonzero_encoding_zero_source", request(a, 2, 1, 0))
          access("zero_encoding_poison_source", request(a, 3, 0, u64))
          access("same_value", request(a, data = model.read(a)))
        }
        for (a <- Seq(0xbc3,0xbc7,0x9e0,0x9e4,0x881,0x88f,0x8b0,0x8b1,0x8b2,0x8b3,0x8bf,0x8c9,0,0xfff);
          op <- Seq(1,2,3,5,6,7)) access("unknown", request(a, op, 1, u64))
        for (op <- Seq(0,4)) access("unknown_operation", request(0x880, op, 1, u64))
        for (a <- combined; rst <- Seq(false,true); cancelPhase <- 0 to 2) {
          access("cancel_seed", request(a, data = u64))
          if (cancelPhase > 0) tick("cancel_accept", Some(request(a)))
          if (cancelPhase > 1) tick("cancel_commit", ready = false)
          tick("cancel_matrix", Some(request(0xbc4, data = 0x7ff)), ready = false, cancel = !rst, reset = rst)
          tick("cancel_recovery")
        }
        val stream = (0 until 256).map { i => request(combined(i % 48), Seq(1,2,3,5,6,7)(i % 6),
          i % 32, (u64-i*73) & u64, i % 32, i % 7 != 0, i % 11 != 0) }
        var index = 0
        while (index < stream.size || phase != 0) {
          val offer = if (index < stream.size) Some(stream(index)) else None
          if (tick("continuous", offer, ready = csrCycles % 5 != 0)) index += 1
        }
        val random = new Random(seed ^ 0xA55AL)
        for (_ <- 0 until randomCases) {
          val q = request(combined(random.nextInt(48)), Seq(1,2,3,5,6,7)(random.nextInt(6)),
            random.nextInt(32), BigInt(64,random), random.nextInt(32), random.nextInt(17) != 0, random.nextInt(17) != 0)
          assert(tick("random_accept", Some(q), ready = false))
          val cancelBefore = random.nextInt(29) == 0
          tick("random_commit", ready = false, cancel = cancelBefore)
          if (!cancelBefore) {
            for (_ <- 0 until random.nextInt(3)) tick("random_hold", ready = false)
            tick("random_response", cancel = random.nextInt(31) == 0)
          }
          if (random.nextInt(127) == 0) tick("random_reset", reset = true)
        }
        assert(phase == 0 && pending.isEmpty && accepts == responses + cancelled)
        tick("csr_final_reset", reset = true)
      }
    } finally { csrTrace.close() }
    assert(immediateCases.count(x => addresses.contains(x._1)) == 46*3*32)
    def obj(m: mutable.Map[String,Int]): String = m.toSeq.sortBy(_._1).map { case (k,v) => s"\"$k\":$v" }.mkString("{",",","}")
    val result = s"""{"status":"PASS","seed":$seed,"randomCasesPerPath":$randomCases,
      "c03Addresses":46,"c03Backings":46,"combinedAddresses":48,"combinedBackings":47,
      "directCycles":$directCycles,"directWrites":$directWrites,"csrCycles":$csrCycles,"csrWrites":$csrWrites,
      "accepted":$accepts,"responses":$responses,"cancelled":$cancelled,"remainingInflight":0,
      "c03ImmediateMatrixCases":${46*3*32},"categories":${obj(categories)},"events":${obj(events)},
      "traceValues":"observed DUT reads, effects and pre/post state; independently checked before recording",
      "scope":"Local native CSR groups and test adapters; no production NewCSR, permissions, retirement or physical timing"}
      """
    Files.write(root.resolve("result.json"), result.getBytes(StandardCharsets.UTF_8))
  }
}
