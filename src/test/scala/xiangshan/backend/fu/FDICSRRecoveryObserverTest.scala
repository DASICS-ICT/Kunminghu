// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._

// The shared identity state is observed without bypassing its allocation or retirement ports.
class FDICSRRecoveryObserverHarness(implicit p: Parameters) extends FDIReferenceCSRHarness {
  val recovery = IO(new Bundle {
    val flushAfter = Input(Bool())
    val live = Output(Vec(RobSize, Bool()))
    val flags = Output(Vec(RobSize, Bool()))
    val ids = Output(Vec(RobSize, UInt(64.W)))
    val canceled = Output(Vec(RobSize, Bool()))
    val responseFlushPipe = Output(Bool())
    val responseFlag = Output(Bool())
  })
  csr.io.flush.bits.level := Mux(recovery.flushAfter, RedirectLevel.flushAfter, RedirectLevel.flush)
  recovery.live := observe(identities.live)
  recovery.flags := observe(identities.flags)
  recovery.ids := observe(identities.ids)
  recovery.canceled := VecInit(identities.io.canceled.map(_.valid))
  recovery.responseFlushPipe := csr.io.out.bits.ctrl.flushPipe.get
  recovery.responseFlag := csr.io.out.bits.ctrl.robIdx.flag
}

class FDICSRRecoveryObserverTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDI recovery shared identities"

  private def clear(data: Data): Unit = data match {
    case value: Bool => value.poke(false.B)
    case value: UInt => value.poke(0.U)
    case value: SInt => value.poke(0.S)
    case value: Vec[_] => value.foreach(clear)
    case value: Record => value.elements.values.foreach(clear)
    case _ => throw new IllegalArgumentException(s"Unsupported observation input $data")
  }

  it should "c07-observer preserve the source through flushAfter and distinguish wrapped identities" in {
    val root = java.nio.file.Paths.get(sys.props("c07.runRoot")).toRealPath()
    require(java.nio.file.Paths.get("").toRealPath() == root)
    val base = new top.DefaultConfig
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = true)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
    test(new FDICSRRecoveryObserverHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("c07-observer-identities"))) { dut =>
      clear(dut.io.request.bits)
      clear(dut.observation.allocate)
      clear(dut.observation.retire)
      clear(dut.observation.boundary)
      dut.io.request.valid.poke(false.B)
      dut.io.response.ready.poke(true.B)
      dut.io.flush.poke(false.B)
      dut.io.flushRob.poke(4.U)
      dut.io.trap.poke(false.B)
      dut.observation.requestFlag.poke(false.B)
      dut.observation.flushFlag.poke(false.B)
      dut.recovery.flushAfter.poke(true.B)
      dut.reset.poke(true.B)
      dut.clock.step(3)
      dut.reset.poke(false.B)

      val pre = scala.collection.mutable.Map.empty[BigInt, Int].withDefaultValue(0)
      val post = scala.collection.mutable.Map.empty[BigInt, Int].withDefaultValue(0)
      val retired = scala.collection.mutable.Map.empty[BigInt, Int].withDefaultValue(0)
      val canceled = scala.collection.mutable.Map.empty[BigInt, Int].withDefaultValue(0)
      var allowedCancel = Option.empty[BigInt]
      var cycle = 0
      var writeEffects = 0
      var nextUid = BigInt(1)
      val word = BigInt("f123456789abcdef", 16)
      val packets = dut.observation.packets

      def edge(): Unit = {
        if (packets(0).valid.peek().litToBoolean && packets(0).kind.peek().litValue == 2) {
          val uid = packets(0).uid.peek().litValue
          pre(uid) += 1
        }
        if (packets(1).valid.peek().litToBoolean) {
          packets(1).kind.expect(3.U)
          val uid = packets(1).uid.peek().litValue
          post(uid) += 1
        }
        packets(2).valid.expect(false.B)
        dut.recovery.canceled.foreach(_.expect(false.B))
        if (packets(3).valid.peek().litToBoolean) {
          val uid = packets(3).uid.peek().litValue
          assert(allowedCancel.contains(uid), s"Unexpected FDI cancellation for UID $uid")
          packets(3).kind.expect(5.U)
          packets(3).actualWrite.expect(false.B)
          dut.observation.retired.valid.expect(false.B)
          canceled(uid) += 1
        }
        if (dut.observation.retired.valid.peek().litToBoolean) {
          packets(3).valid.expect(false.B)
          val uid = dut.observation.retired.uid.peek().litValue
          retired(uid) += 1
          assert(retired(uid) == 1, s"Duplicate retirement for UID $uid")
        }
        if (dut.io.writes.peek().litValue != 0) {
          dut.io.writes.expect((BigInt(1) << 40).U)
          writeEffects += 1
        }
        dut.clock.step()
        cycle += 1
      }

      packets(0).valid.expect(true.B)
      packets(0).kind.expect(1.U)
      edge()
      packets.foreach(_.valid.expect(false.B))

      def allocate(slot: Int, flag: Boolean, instruction: BigInt, pc: BigInt): BigInt = {
        val uid = nextUid
        nextUid += 1
        val port = dut.observation.allocate
        port.valid.poke(true.B)
        port.bits.ptr.value.poke(slot.U)
        port.bits.ptr.flag.poke(flag.B)
        port.bits.pc.poke(pc.U)
        port.bits.instr.poke(instruction.U)
        edge()
        port.valid.poke(false.B)
        dut.recovery.live(slot).expect(true.B)
        dut.recovery.flags(slot).expect(flag.B)
        dut.recovery.ids(slot).expect(uid.U)
        uid
      }

      def access(flag: Boolean, write: Boolean, oldWord: BigInt): BigInt = {
        val function = if (write) 1 else 2
        val instruction = (BigInt(0x8b1) << 20) | (if (write) BigInt(1) << 15 else BigInt(0)) |
          (BigInt(function) << 12) | (BigInt(1) << 7) | 0x73
        val pc = BigInt(0x1000) + nextUid * 4
        val uid = allocate(4, flag, instruction, pc)
        dut.observation.requestFlag.poke(flag.B)
        dut.io.request.bits.instruction.poke(instruction.U)
        dut.io.request.bits.operation.poke((8 | function).U)
        dut.io.request.bits.operand.poke((if (write) word else BigInt(0)).U)
        dut.io.request.bits.basePc.poke((pc & ~BigInt(31)).U)
        dut.io.request.bits.offset.poke(((pc & 31) >> 1).U)
        dut.io.request.bits.rob.poke(4.U)
        dut.io.request.valid.poke(true.B)
        dut.io.request.ready.expect(true.B)
        packets(0).valid.expect(true.B)
        packets(0).kind.expect(2.U)
        packets(0).uid.expect(uid.U)
        packets(0).robIdx.expect(4.U)
        packets(0).robFlag.expect(flag.B)
        packets(0).pc.expect(pc.U)
        packets(0).instruction.expect(instruction.U)
        packets(0).writeNeeded.expect(write.B)
        packets(0).permitted.expect(true.B)
        packets(0).words(40).expect(oldWord.U)
        edge()
        dut.io.request.valid.poke(false.B)
        dut.io.response.valid.expect(true.B)
        dut.io.response.bits.illegal.expect(false.B)
        dut.io.response.bits.virtualIllegal.expect(false.B)
        dut.io.response.bits.rob.expect(4.U)
        dut.io.response.bits.data.expect(oldWord.U)
        dut.recovery.responseFlag.expect(flag.B)
        dut.recovery.responseFlushPipe.expect(write.B)
        dut.io.writes.expect((if (write) BigInt(1) << 40 else BigInt(0)).U)
        edge()
        dut.io.response.valid.expect(false.B)
        dut.io.writes.expect(0.U)
        dut.io.state(40).expect(word.U)
        packets(1).valid.expect(true.B)
        packets(1).uid.expect(uid.U)
        packets(1).robIdx.expect(4.U)
        packets(1).robFlag.expect(flag.B)
        packets(1).actualWrite.expect(write.B)
        packets(1).responseIllegal.expect(false.B)
        packets(1).responseVirtual.expect(false.B)
        packets(1).words(40).expect(word.U)
        edge()
        packets.foreach(_.valid.expect(false.B))
        uid
      }

      def redirect(flag: Boolean): Unit = {
        dut.io.flushRob.poke(4.U)
        dut.observation.flushFlag.poke(flag.B)
        dut.recovery.flushAfter.poke(true.B)
        dut.io.flush.poke(true.B)
      }

      def retire(uid: BigInt, flag: Boolean): Unit = {
        // This fixture only accepts the legal equal-flushAfter coincidence at its interface.
        if (dut.io.flush.peek().litToBoolean) {
          dut.io.flushRob.expect(4.U)
          dut.observation.flushFlag.expect(flag.B)
          dut.recovery.flushAfter.expect(true.B)
        }
        dut.observation.retire.valid.poke(true.B)
        dut.observation.retire.bits.ptr.value.poke(4.U)
        dut.observation.retire.bits.ptr.flag.poke(flag.B)
        dut.observation.retire.bits.count.poke(1.U)
        dut.observation.retired.uid.expect(uid.U)
        dut.observation.retired.robIdx.expect(4.U)
        dut.observation.retired.robFlag.expect(flag.B)
        edge()
        dut.observation.retire.valid.poke(false.B)
        dut.recovery.live(4).expect(false.B)
      }

      // Younger allocations here exercise the observer API, not the real ROB's CSR serialization.
      for ((flag, sameEdge) <- Seq((false, false), (true, true))) {
        val uid = access(flag, write = true, oldWord = if (flag) word else BigInt(0))
        val younger = allocate(7, flag, BigInt(0x13), BigInt(0x2000))
        assert(younger != uid)
        redirect(flag)
        packets(3).valid.expect(false.B)
        dut.recovery.live(4).expect(true.B)
        dut.recovery.ids(4).expect(uid.U)
        if (sameEdge) {
          // Actual ROB redirect blocks commit; same-edge retirement is only an observer property.
          retire(uid, flag)
        } else {
          edge()
          dut.recovery.live(4).expect(true.B)
          dut.recovery.flags(4).expect(flag.B)
          dut.recovery.ids(4).expect(uid.U)
        }
        dut.recovery.live(7).expect(false.B)
        dut.io.flush.poke(false.B)
        if (!sameEdge) { retire(uid, flag) }
        edge()
        println(s"C07_OBSERVER_SOURCE uid=$uid flag=$flag sameEdge=$sameEdge youngerUid=$younger " +
          "pre=1 post=1 retire=1 cancel=0 youngerLive=false youngerCancel=false")
      }

      // A pure read has no applied write: the opposite flag at the same value is a real cancellation.
      val readUid = access(flag = false, write = false, oldWord = word)
      allowedCancel = Some(readUid)
      redirect(flag = true)
      packets(3).valid.expect(true.B)
      packets(3).uid.expect(readUid.U)
      packets(3).robIdx.expect(4.U)
      packets(3).robFlag.expect(false.B)
      packets(3).cancelReason.expect(1.U)
      edge()
      dut.io.flush.poke(false.B)
      allowedCancel = None
      dut.recovery.live(4).expect(false.B)
      edge()

      val freshUid = access(flag = true, write = false, oldWord = word)
      assert(freshUid != readUid && !retired.contains(freshUid))
      redirect(flag = true)
      packets(3).valid.expect(false.B)
      edge()
      dut.io.flush.poke(false.B)
      dut.recovery.live(4).expect(true.B)
      dut.recovery.ids(4).expect(freshUid.U)
      retire(freshUid, flag = true)
      edge()
      edge()
      assert(pre.toMap == Map(BigInt(1) -> 1, BigInt(3) -> 1, BigInt(5) -> 1, BigInt(6) -> 1))
      assert(post.toMap == pre.toMap)
      assert(retired.toMap == Map(BigInt(1) -> 1, BigInt(3) -> 1, BigInt(6) -> 1))
      assert(canceled.toMap == Map(BigInt(5) -> 1))
      assert(writeEffects == 2)
      packets.foreach(_.valid.expect(false.B))
      dut.recovery.live.foreach(_.expect(false.B))
      println(s"C07_OBSERVER_PASS cycles=$cycle writes=$writeEffects pre=4 post=4 retire=3 " +
        "pureReadCancel=1 sourceWriteCancel=0 sharedIdentity=true physicalRobTiming=false")
    }
  }
}
