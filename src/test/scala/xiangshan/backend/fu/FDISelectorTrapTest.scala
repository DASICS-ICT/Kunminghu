// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import utility.ClockGate
import xiangshan._
import xiangshan.backend.Bundles.ExceptionInfo
import xiangshan.backend.decode.Imm_Z
import xiangshan.backend.exu.ExeUnit
import xiangshan.backend.fu.NewCSR.FDIFReasonModule
import xiangshan.backend.fu.wrapper.CSR
import xiangshan.backend.rob.{FDIExceptionInjection, FDIExceptionInjectionHarness, RobPtr}

// The test supplies the precise ROB acceptance and its source-PC lookup.
// Age selection and the two-cycle CSR transport remain production modules.
class FDISelectorTrapHarness(implicit p: Parameters) extends LazyModule {
  private val backend = p(XSCoreParamsKey).backendParams
  backend.allSchdParams.foreach(_.bindBackendParam(backend))
  backend.allIssueParams.foreach(_.bindBackendParam(backend))
  backend.allExuParams.zipWithIndex.foreach { case (exu, index) =>
    exu.bindBackendParam(backend)
    exu.updateIQWakeUpConfigs(backend.iqWakeUpParams)
    exu.updateExuIdx(index)
  }
  private val csrParams = backend.allExuParams.find(_.hasCSR).get
  val csrUnit = LazyModule(new ExeUnit(csrParams))

  lazy val module = new FDISelectorTrapHarnessImp(this)

  class FDISelectorTrapHarnessImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasXSParameter {
    val selector = Module(new FDIExceptionInjectionHarness)
    val exu = csrUnit.module
    val io = IO(new Bundle {
      val request = Flipped(Decoupled(new Bundle {
        val instruction = UInt(32.W)
        val operation = UInt(6.W)
        val operand = UInt(64.W)
      }))
      val response = Decoupled(new Bundle {
        val data = UInt(64.W)
        val illegal = Bool()
      })
      val wb = Input(Vec(backend.allExuParams.count(_.exceptionOut.nonEmpty), Valid(new FDIExceptionInjection)))
      val redirect = Input(Valid(new Bundle {
        val robIdx = new RobPtr
        val flushItself = Bool()
      }))
      val acceptSelected = Input(Bool())
      val sourcePc = Input(UInt(64.W))
      val selected = Output(Valid(new Bundle {
        val robIdx = new RobPtr
        val vector = UInt(26.W)
        val tval = UInt(64.W)
        val reason = UInt(3.W)
      }))
      val transportedValid = Output(Bool())
      val state = Output(Vec(FDITrapTestAddresses.all.size, UInt(64.W)))
      val mode = Output(UInt(2.W))
      val hardwareReasonWrite = Output(Bool())
      val hardwareReasonValue = Output(UInt(3.W))
      val monitorReset = Input(Bool())
      val resetClockEnable = Output(Bool())
      val gatedResetEdges = Output(UInt(8.W))
      val dividerReady = Output(Bool())
    })
    def tieInputs(data: Data): Unit = data match {
      case record: Record => record.elements.values.foreach(tieInputs)
      case vector: Vec[_] => vector.foreach(tieInputs)
      case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
      case _ =>
    }
    tieInputs(exu.io)
    exu.io.csrio.get.userTimerDelivery.foreach(_.entry.completion.ready := true.B)
    selector.io.wb := io.wb
    selector.io.redirect := io.redirect
    selector.io.enq.foreach { input => input.valid := false.B; input.bits := 0.U.asTypeOf(input.bits) }
    selector.io.filterInput := 0.U
    val accept = io.acceptSelected && selector.io.state.valid
    when(io.acceptSelected) { assert(selector.io.state.valid, "A precise boundary requires a selected exception") }
    selector.io.flush := accept
    io.selected.valid := selector.io.state.valid
    io.selected.bits.robIdx := selector.io.state.bits.robIdx
    io.selected.bits.vector := selector.io.state.bits.exceptionVec.asUInt
    io.selected.bits.tval := selector.io.state.bits.fdiException.get.tval
    io.selected.bits.reason := selector.io.state.bits.fdiException.get.reason

    val selected = WireInit(0.U.asTypeOf(new ExceptionInfo))
    selected.pc := io.sourcePc
    selected.instr := 0x00000013.U
    selected.exceptionVec := selector.io.state.bits.exceptionVec
    selected.fdiException.get := selector.io.state.bits.fdiException.get
    // This capture models ROB's accepted output boundary only; no age or
    // exception arbitration is recreated by the integration harness.
    exu.io.csrio.get.exception.valid := RegNext(accept, false.B)
    exu.io.csrio.get.exception.bits := RegEnable(selected, accept)

    exu.io.in.valid := io.request.valid
    exu.io.in.bits.fuType := FuType.csr.U
    exu.io.in.bits.fuOpType := io.request.bits.operation
    exu.io.in.bits.imm := Imm_Z().minBitsFromInstr(io.request.bits.instruction)
    exu.io.in.bits.src(0) := io.request.bits.operand
    exu.io.in.bits.pc.foreach(_ := 0x1000.U)
    exu.io.in.bits.robIdx.value := 4.U
    exu.io.in.bits.rfWen.foreach(_ := true.B)
    io.request.ready := exu.io.in.ready
    exu.io.out.ready := io.response.ready
    io.response.valid := exu.io.out.valid
    io.response.bits.data := exu.io.out.bits.data.head
    io.response.bits.illegal := exu.io.out.bits.exceptionVec.get(2)

    val csr = exu.funcUnits.collectFirst { case unit: CSR => unit }.get
    FDITrapTestAddresses.all.zipWithIndex.foreach { case (address, index) =>
      io.state(index) := csr.csrMod.csrOutMap.get(address).map(BoringUtils.bore(_)).getOrElse(0.U)
    }
    val reason = csr.csrMod.csrMods.find(_.addr == 0x8b3).get.asInstanceOf[FDIFReasonModule]
    io.hardwareReasonWrite := BoringUtils.bore(reason.trapReason.valid)
    io.hardwareReasonValue := BoringUtils.bore(reason.trapReason.bits.REASON).asUInt
    io.transportedValid := BoringUtils.bore(csr.io.csrio.get.exception.valid)
    io.mode := BoringUtils.bore(csr.csrMod.io.status.privState.PRVM).asUInt

    // This local simulator infers synchronous resets. Open the native test
    // enable only during reset so an idle gated FU also samples that reset.
    // The full core uses its existing asynchronous reset chain instead.
    val fixtureReset = reset.asBool
    val clockTest = ClockGate.genTeSrc
    clockTest.cgen := fixtureReset
    io.resetClockEnable := clockTest.cgen
    val divider = exu.funcUnits.find(_.cfg.name == "div").get
    val dividerClock = BoringUtils.bore(divider.clock)
    io.dividerReady := BoringUtils.bore(divider.io.in.ready)
    // A separate literal async reset initializes only this test monitor. Its
    // counter advances on the actual gated clock while the DUT reset is high.
    io.gatedResetEdges := withClockAndReset(dividerClock, io.monitorReset.asAsyncReset) {
      val edges = RegInit(0.U(8.W))
      when(fixtureReset) { edges := edges + 1.U }
      edges
    }
  }
}

class FDISelectorTrapTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDI selector and production CSR transport"

  it should "consume the selected record once across the production execution-unit pipeline" in {
    val root = java.nio.file.Paths.get(sys.props("e02.runRoot")).toRealPath()
    require(java.nio.file.Paths.get("").toRealPath() == root)
    val (base, _, _) = top.ArgParser.parse(Array(
      "--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768",
      "--fpga-platform", "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = true)
    }
    val exus = p(XSCoreParamsKey).backendParams.allExuParams.filter(_.exceptionOut.nonEmpty)
    val load = exus.indexWhere(_.fuConfigs.exists(_.name == "ldu"))
    val jump = exus.indexWhere(_.fuConfigs.exists(_.name == "jmp"))
    val system = exus.indexWhere(_.fuConfigs.exists(_.name == "csr"))
    require(Seq(load, jump, system).forall(_ >= 0))
    test(LazyModule(new FDISelectorTrapHarness).module).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("selector-trap-production"))) { dut =>
      var cycles = 0
      var hardwareEffects = 0
      var transportEvents = 0
      var resetting = true
      def step(): Unit = {
        if (!resetting) {
          dut.io.resetClockEnable.expect(false.B)
          if (dut.io.hardwareReasonWrite.peek().litToBoolean) hardwareEffects += 1
          if (dut.io.transportedValid.peek().litToBoolean) transportEvents += 1
        }
        dut.clock.step()
        cycles += 1
      }
      def raw(address: Int): BigInt = dut.io.state(FDITrapTestAddresses.all.indexOf(address)).peek().litValue
      def idleFaults(): Unit = dut.io.wb.foreach { input =>
        input.valid.poke(false.B)
        input.bits.robIdx.flag.poke(false.B); input.bits.robIdx.value.poke(0.U)
        input.bits.standard.poke(0.U); input.bits.denied.poke(false.B)
        input.bits.privilege.poke(0.U); input.bits.virtual.poke(false.B)
        input.bits.tval.poke(0.U); input.bits.reason.poke(0.U)
        input.bits.replay.poke(false.B); input.bits.flushPipe.poke(false.B)
        input.bits.vector.poke(false.B); input.bits.vstart.poke(0.U); input.bits.vuopIdx.poke(0.U)
      }
      def instruction(word: BigInt, operation: UInt, operand: BigInt): BigInt = {
        dut.io.request.bits.instruction.poke(word.U)
        dut.io.request.bits.operation.poke(operation)
        dut.io.request.bits.operand.poke(operand.U)
        dut.io.request.valid.poke(true.B)
        var waited = 0
        while (!dut.io.request.ready.peek().litToBoolean && waited < 16) { step(); waited += 1 }
        dut.io.request.ready.expect(true.B)
        step()
        dut.io.request.valid.poke(false.B)
        waited = 0
        while (!dut.io.response.valid.peek().litToBoolean && waited < 16) { step(); waited += 1 }
        dut.io.response.valid.expect(true.B)
        dut.io.response.bits.illegal.expect(false.B)
        val result = dut.io.response.bits.data.peek().litValue
        step()
        result
      }
      def write(address: Int, value: BigInt): Unit = {
        instruction((BigInt(address) << 20) | (1 << 15) | (1 << 12) | (1 << 7) | 0x73, 9.U, value)
      }
      def mret(): Unit = { instruction(BigInt("30200073", 16), CSROpType.jmp, 0); dut.io.mode.expect(0.U) }
      def fault(port: Int, rob: Int, value: BigInt, reason: Int, standard: BigInt = 0): Unit = {
        val input = dut.io.wb(port)
        input.valid.poke(true.B)
        input.bits.robIdx.value.poke(rob.U)
        input.bits.denied.poke(true.B)
        input.bits.tval.poke(value.U)
        input.bits.reason.poke(reason.U)
        input.bits.standard.poke(standard.U)
      }
      def selected(rob: Int, value: BigInt, reason: Int): Unit = {
        dut.io.selected.valid.expect(true.B)
        dut.io.selected.bits.robIdx.value.expect(rob.U)
        dut.io.selected.bits.tval.expect(value.U)
        dut.io.selected.bits.reason.expect(reason.U)
      }
      def retire(pc: BigInt, cause: Int, value: BigInt, expectedReason: Int, newEffect: Boolean): Unit = {
        val previous = Seq(0x341, 0x342, 0x343, 0x8b3).map(a => a -> raw(a)).toMap
        val effectsBefore = hardwareEffects
        val transportsBefore = transportEvents
        dut.io.sourcePc.poke(pc.U)
        dut.io.acceptSelected.poke(true.B)
        step()
        dut.io.acceptSelected.poke(false.B)
        // Change the live lookup after acceptance. The captured record, not
        // this new value or later selector activity, owns the trap metadata.
        dut.io.sourcePc.poke(BigInt("8000f006", 16).U)
        for (_ <- 0 until 2) {
          previous.foreach { case (address, old) => assert(raw(address) == old) }
          step()
        }
        previous.foreach { case (address, old) => assert(raw(address) == old) }
        dut.io.transportedValid.expect(true.B)
        dut.io.hardwareReasonWrite.expect(newEffect.B)
        if (newEffect) dut.io.hardwareReasonValue.expect(expectedReason.U)
        step()
        assert(raw(0x341) == pc)
        assert(raw(0x342) == cause)
        if (cause == 24) assert(raw(0x343) == value)
        assert(raw(0x8b3) == expectedReason)
        assert(hardwareEffects == effectsBefore + (if (newEffect) 1 else 0))
        assert(transportEvents == transportsBefore + 1)
        for (_ <- 0 until 3) { dut.io.hardwareReasonWrite.expect(false.B); step() }
        assert(hardwareEffects == effectsBefore + (if (newEffect) 1 else 0))
      }

      dut.io.request.valid.poke(false.B)
      dut.io.request.bits.instruction.poke(0.U)
      dut.io.request.bits.operation.poke(0.U)
      dut.io.request.bits.operand.poke(0.U)
      dut.io.response.ready.poke(true.B)
      dut.io.acceptSelected.poke(false.B)
      dut.io.sourcePc.poke(0.U)
      dut.io.redirect.valid.poke(false.B)
      dut.io.redirect.bits.robIdx.value.poke(0.U)
      dut.io.redirect.bits.robIdx.flag.poke(false.B)
      dut.io.redirect.bits.flushItself.poke(true.B)
      idleFaults()
      dut.io.monitorReset.poke(true.B)
      dut.reset.poke(true.B)
      dut.io.resetClockEnable.expect(true.B)
      step()
      dut.io.monitorReset.poke(false.B)
      for (_ <- 0 until 4) step()
      dut.io.gatedResetEdges.expect(4.U)
      dut.reset.poke(false.B)
      resetting = false
      step()
      dut.io.resetClockEnable.expect(false.B)
      dut.io.dividerReady.expect(true.B)
      dut.io.request.ready.expect(true.B)
      write(0x300, 0)
      write(0x305, BigInt("80000100", 16))
      write(0x341, BigInt("80001000", 16))
      write(0x8b3, 7)
      mret()

      val first = BigInt("fedcba9876543213", 16)
      fault(jump, 19, BigInt("80003337", 16), 4)
      fault(load, 7, first, 2)
      step(); idleFaults(); step(); step()
      selected(7, first, 2)
      retire(BigInt("80001202", 16), 24, first, 2, newEffect = true)
      mret()

      val second = BigInt("8000000100000007", 16)
      fault(jump, 29, second, 4)
      step(); idleFaults(); step(); step()
      selected(29, second, 4)
      retire(BigInt("80001406", 16), 24, second, 4, newEffect = true)
      mret()

      fault(jump, 35, BigInt("fffffffffffffffd", 16), 4)
      step(); idleFaults(); step(); step()
      selected(35, BigInt("fffffffffffffffd", 16), 4)
      dut.io.redirect.valid.poke(true.B)
      dut.io.redirect.bits.robIdx.value.poke(35.U)
      step()
      dut.io.redirect.valid.poke(false.B)
      for (_ <- 0 until 4) { dut.io.hardwareReasonWrite.expect(false.B); step() }
      dut.io.selected.valid.expect(false.B)
      assert(raw(0x8b3) == 4 && hardwareEffects == 2 && transportEvents == 2)

      fault(system, 41, 0, 1, standard = BigInt(1) << 2)
      step(); idleFaults(); step(); step()
      selected(41, 0, 1)
      retire(BigInt("8000180a", 16), 2, 0, 4, newEffect = false)
      assert(hardwareEffects == 2 && transportEvents == 3)
      dut.io.gatedResetEdges.expect(4.U)
      println(s"FDI selector trap PASS cycles=$cycles selectedTraps=3 canceled=1 " +
        s"hardwareEffects=$hardwareEffects transportEvents=$transportEvents selector=production exu=production")
    }
  }
}
