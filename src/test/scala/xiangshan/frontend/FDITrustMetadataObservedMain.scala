// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.frontend

import chisel3._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import freechips.rocketchip.diplomacy.DisableMonitors
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator, SimTop}
import utility.{ChiselDB, Constantin, FileRegisters}
import xiangshan.{CustomCSRCtrlIO, DebugOptionsKey, TlbCsrBundle, XSTileKey}

// Outputs only: the production hierarchy retains every clock, reset and handshake.
class FDITrustMetadataObservedSimTop(implicit p: Parameters) extends SimTop {
  override def desiredName: String = "SimTop"

  override protected def addDifftestObservations(): Unit = {
    val core = l_soc.core_with_l2.head.core
    val frontend = core.frontend.inner.module
    val ifu = frontend.ifu
    val ibuffer = frontend.ibuffer
    val backend = core.backend.inner.module
    val ctrl = core.backend.inner.ctrlBlock.module
    val memory = core.memBlock.inner.module
    val enabled = core.coreParams.HasFDI

    def tap[T <: Data](name: String, source: T): Unit = {
      val output = IO(Output(chiselTypeOf(source))).suggestName(s"c08_observed_$name")
      output := observe(source)
    }

    def tag(name: String, source: Option[Bool]): Unit = {
      require(source.isDefined == enabled, s"Unexpected optional source tag at $name")
      val output = IO(Output(Bool())).suggestName(s"c08_observed_${name}_tag")
      // Disabled outputs are observer constants, never replacement production tags.
      output := source.map(value => observe(value)).getOrElse(false.B)
    }

    def context(name: String, tlb: TlbCsrBundle, control: CustomCSRCtrlIO): Unit = {
      tap(s"${name}_imode", tlb.priv.imode)
      tap(s"${name}_source_v", control.virtMode)
      tap(s"${name}_satp_mode", tlb.satp.mode)
      tap(s"${name}_vsatp_mode", tlb.vsatp.mode)
      // Data mode is observed only to distinguish it from the actual fetch source.
      tap(s"${name}_dmode", tlb.priv.dmode)
      tap(s"${name}_dvirt", tlb.priv.virt)
    }

    val featurePresent = IO(Output(Bool())).suggestName("c08_observed_feature_present")
    featurePresent := enabled.B
    context("backend_export", backend.io.frontendTlbCsr, backend.io.frontendCsrCtrl)
    context("frontend_input", frontend.io.tlbCsr, frontend.io.csrCtrl)
    context("frontend_delayed", frontend.tlbCsr, frontend.csrCtrl)
    tap("distribution_input", frontend.io.csrCtrl.distribute_csr)
    tap("distribution_delayed", frontend.csrCtrl.distribute_csr)
    require(frontend.fdiMirror.isDefined == enabled && ifu.io.fdiConfig.isDefined == enabled)
    if (enabled) {
      val mirror = frontend.fdiMirror.get
      require(mirror.client.addresses.size == 5)
      tap("frontend_mirror_input", mirror.io.distribute)
      tap("frontend_mirror_words", mirror.io.state)
      tap("ifu_config", ifu.io.fdiConfig.get)
    }

    tap("f2_valid", ifu.f2_valid)
    tap("f2_fire", ifu.f2_fire)
    tap("f2_flush", ifu.f2_flush)
    tap("f2_pc", ifu.f2_pc)
    tap("f2_ftq", ifu.f2_ftq_req.ftqIdx)
    tap("f3_valid", ifu.f3_valid)
    tap("f3_flush", ifu.f3_flush)
    tap("f3_pc", ifu.f3_pc)
    tap("f3_ftq", ifu.f3_ftq_req.ftqIdx)
    tap("fetch_valid", ifu.io.toIbuffer.valid)
    tap("fetch_ready", ifu.io.toIbuffer.ready)
    tap("fetch_starts", ifu.io.toIbuffer.bits.valid)
    tap("fetch_enables", ifu.io.toIbuffer.bits.enqEnable)
    tap("ibuffer_flush", ibuffer.io.flush)
    for (lane <- ifu.f2_pc.indices) {
      tag(s"f2_$lane", ifu.f2_fdiNotTrusted.map(_(lane)))
      tag(s"f3_$lane", ifu.f3_fdiNotTrusted.map(_(lane)))
      tag(s"fetch_$lane", ifu.io.toIbuffer.bits.fdiNotTrusted.map(_(lane)))
      tag(s"ibuffer_input_$lane", ibuffer.io.in.bits.fdiNotTrusted.map(_(lane)))
    }
    ibuffer.io.out.zipWithIndex.foreach { case (port, lane) =>
      tap(s"ibuffer_${lane}_valid", port.valid)
      tap(s"ibuffer_${lane}_ready", port.ready)
      tap(s"ibuffer_${lane}_pc", port.bits.pc)
      tap(s"ibuffer_${lane}_instruction", port.bits.instr)
      tag(s"ibuffer_$lane", port.bits.fdiNotTrusted)
      tag(s"decode_input_$lane", ctrl.decode.io.in(lane).bits.fdiNotTrusted)
    }
    ctrl.decode.io.out.zipWithIndex.foreach { case (port, lane) =>
      tap(s"decode_${lane}_valid", port.valid)
      tag(s"decode_$lane", port.bits.fdiNotTrusted)
    }
    ctrl.rename.io.out.zipWithIndex.foreach { case (port, lane) =>
      tap(s"rename_${lane}_valid", port.valid)
      tap(s"rename_${lane}_ready", port.ready)
      tap(s"rename_${lane}_rob", port.bits.robIdx)
      tag(s"rename_$lane", port.bits.fdiNotTrusted)
    }
    require(ctrl.fusionDecoder.io.fdiNotTrusted.isDefined == enabled)
    ctrl.fusionDecoder.io.in.indices.foreach { lane =>
      tag(s"fusion_input_$lane", ctrl.fusionDecoder.io.fdiNotTrusted.map(_(lane)))
    }
    tap("fusion_output", ctrl.fusionDecoder.io.out)
    tap("fusion_clear", ctrl.fusionDecoder.io.clear)
    tap("fusion_info", ctrl.fusionDecoder.io.info)

    val queues = Seq(core.backend.inner.intScheduler, core.backend.inner.fpScheduler,
      core.backend.inner.vfScheduler, core.backend.inner.memScheduler).flatten
      .flatMap(_.issueQueue).map(_.module)
    queues.zipWithIndex.foreach { case (queue, index) =>
      queue.io.enq.zipWithIndex.foreach { case (port, lane) =>
        tap(s"iq_${index}_${lane}_enqueue_valid", port.valid)
        tag(s"iq_${index}_${lane}_enqueue", port.bits.fdiNotTrusted)
      }
      queue.deqBeforeDly.zipWithIndex.foreach { case (port, lane) =>
        tap(s"iq_${index}_${lane}_issue_valid", port.valid)
        tap(s"iq_${index}_${lane}_issue_rob", port.bits.common.robIdx)
        tag(s"iq_${index}_${lane}_issue", port.bits.common.fdiNotTrusted)
      }
    }
    val units = Seq(core.backend.inner.intExuBlock, core.backend.inner.fpExuBlock,
      core.backend.inner.vfExuBlock).flatten.flatMap(_.exus).map(_.module)
    units.zipWithIndex.foreach { case (unit, index) =>
      tag(s"exu_$index", unit.io.in.bits.fdiNotTrusted)
      unit.funcUnits.zipWithIndex.foreach { case (fu, function) =>
        tap(s"fu_${index}_${function}_valid", fu.io.in.valid)
        tap(s"fu_${index}_${function}_ready", fu.io.in.ready)
        tag(s"fu_${index}_$function", fu.io.in.bits.ctrl.fdiNotTrusted)
        fu.io.in.bits.ctrlPipe.foreach { pipe =>
          pipe.zipWithIndex.foreach { case (control, stage) =>
            tag(s"fu_${index}_${function}_pipe_$stage", control.fdiNotTrusted)
          }
        }
      }
    }
    // Preserve Backend's issue ordering through its public port groups.
    val backendMemoryPorts = (backend.io.mem.issueSta ++
      backend.io.mem.issueHylda ++ backend.io.mem.issueHysta ++
      backend.io.mem.issueLda ++ backend.io.mem.issueVldu ++ backend.io.mem.issueStd).toSeq
    backendMemoryPorts.zipWithIndex.foreach { case (port, lane) =>
      tap(s"backend_memory_${lane}_valid", port.valid)
      tag(s"backend_memory_$lane", port.bits.uop.fdiNotTrusted)
    }
    memory.io.ooo_to_mem.issueUops.zipWithIndex.foreach { case (port, lane) =>
      tap(s"memory_${lane}_valid", port.valid)
      tap(s"memory_${lane}_ready", port.ready)
      tap(s"memory_${lane}_rob", port.bits.uop.robIdx)
      tag(s"memory_$lane", port.bits.uop.fdiNotTrusted)
    }
    // Store-data FUs reside inside MemBlock, outside Backend's execution blocks.
    memory.stdExeUnits.zipWithIndex.foreach { case (unit, index) =>
      tap(s"memory_std_${index}_input_valid", unit.io.in.valid)
      tap(s"memory_std_${index}_input_ready", unit.io.in.ready)
      tap(s"memory_std_${index}_input_rob", unit.io.in.bits.uop.robIdx)
      tag(s"memory_std_${index}_input", unit.io.in.bits.uop.fdiNotTrusted)
      tap(s"memory_std_${index}_fu_valid", unit.fu.io.in.valid)
      tap(s"memory_std_${index}_fu_ready", unit.fu.io.in.ready)
      tap(s"memory_std_${index}_fu_rob", unit.fu.io.in.bits.ctrl.robIdx)
      tag(s"memory_std_${index}_fu", unit.fu.io.in.bits.ctrl.fdiNotTrusted)
    }
  }
}

object FDITrustMetadataObservedMain extends App {
  val (config, firrtlOptions, firtoolOptions) = ArgParser.parse(args)
  require(config(XSTileKey).size == 1)
  require(config(XSTileKey).head.HasVPU && config(XSTileKey).head.VLEN == 128)
  val debug = config(DebugOptionsKey)
  require(debug.FPGAPlatform && !debug.EnableDifftest && !debug.AlwaysBasicDiff,
    "Structural observation must not enable runtime comparison")
  Constantin.init(debug.EnableConstantin && !debug.FPGAPlatform)
  ChiselDB.init(debug.EnableChiselDB && !debug.FPGAPlatform)
  Generator.execute(firrtlOptions,
    DisableMonitors(p => new FDITrustMetadataObservedSimTop()(p))(config), firtoolOptions)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
