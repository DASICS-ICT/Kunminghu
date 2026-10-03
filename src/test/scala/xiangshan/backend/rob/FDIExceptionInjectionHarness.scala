// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.FDIExceptionRecord
import xiangshan.backend.fu.FuConfig

class FDIExceptionInjection(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val standard = UInt(26.W)
  val denied = Bool()
  val privilege = UInt(2.W)
  val virtual = Bool()
  val tval = UInt(64.W)
  val reason = UInt(3.W)
  val replay = Bool()
  val flushPipe = Bool()
  val vector = Bool()
  val vstart = UInt(64.W)
  val vuopIdx = xiangshan.backend.Bundles.UopIdx()
}

// Fault production is injected; age selection, cancellation and current-state
// replacement use the production ROB exception pipeline without a test copy.
class FDIExceptionInjectionHarness(implicit p: Parameters) extends XSModule {
  private val exus = backendParams.allExuParams.filter(_.exceptionOut.nonEmpty)
  private val configs = (FuConfig.allConfigs ++ Seq(FuConfig.FakeHystaCfg) ++ exus.flatMap(_.fuConfigs)).distinct
  private val selector = Module(new ExceptionGen(backendParams))

  val io = IO(new Bundle {
    val wb = Input(Vec(exus.size, Valid(new FDIExceptionInjection)))
    val redirect = Input(Valid(new Bundle {
      val robIdx = new RobPtr
      val flushItself = Bool()
    }))
    val flush = Input(Bool())
    val enq = Input(Vec(RenameWidth, Valid(new FDIExceptionInjection)))
    val state = Output(Valid(new RobExceptionInfo))
    val out = Output(Valid(new RobExceptionInfo))
    val acceptedMasks = Output(Vec(exus.size, UInt(26.W)))
    val filterInput = Input(UInt(26.W))
    val perFuMasks = Output(Vec(configs.size, UInt(26.W)))
    val selectedCause = Output(UInt(6.W))
  })

  selector.io.redirect.valid := io.redirect.valid
  selector.io.redirect.bits := 0.U.asTypeOf(selector.io.redirect.bits)
  selector.io.redirect.bits.robIdx := io.redirect.bits.robIdx
  selector.io.redirect.bits.level := Mux(io.redirect.bits.flushItself,
    RedirectLevel.flush, RedirectLevel.flushAfter)
  selector.io.flush := io.flush
  selector.io.enq.zip(io.enq).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits := 0.U.asTypeOf(sink.bits)
    sink.bits.robIdx := source.bits.robIdx
    sink.bits.exceptionVec := ExceptionNO.selectFrontend(source.bits.standard.asTypeOf(ExceptionVec()))
    sink.bits.hasException := sink.bits.exceptionVec.asUInt.orR
    sink.bits.isEnqExcp := true.B
  }
  selector.io.wb.zip(io.wb).zip(exus).zipWithIndex.foreach {
    case (((sink, source), exu), index) =>
      val raw = WireInit(source.bits.standard.asTypeOf(ExceptionVec()))
      val generated = FDIExceptionRecord.exceptionVector(source.bits.denied,
        source.bits.privilege, source.bits.virtual)
      raw := (source.bits.standard | generated.asUInt).asTypeOf(ExceptionVec())
      val filtered = ExceptionNO.partialSelect(raw, exu.exceptionOut)
      sink.valid := source.valid
      sink.bits := 0.U.asTypeOf(sink.bits)
      sink.bits.robIdx := source.bits.robIdx
      sink.bits.exceptionVec := filtered
      sink.bits.hasException := filtered.asUInt.orR
      sink.bits.replayInst := source.bits.replay
      sink.bits.flushPipe := source.bits.flushPipe
      sink.bits.vstartEn := source.bits.vector
      sink.bits.vstart := source.bits.vstart
      sink.bits.vuopIdx := source.bits.vuopIdx
      sink.bits.fdiException.foreach { record =>
        record.tval := source.bits.tval
        record.reason := source.bits.reason
      }
      io.acceptedMasks(index) := filtered.asUInt
  }

  io.state := selector.io.state
  io.out := selector.io.out
  io.perFuMasks := VecInit(configs.map { config =>
    ExceptionNO.selectByFu(io.filterInput.asTypeOf(ExceptionVec()), config).asUInt
  })
  io.selectedCause := PriorityMux(ExceptionNO.priorities.map { cause =>
    selector.io.state.bits.exceptionVec(cause) -> cause.U(6.W)
  })
}
