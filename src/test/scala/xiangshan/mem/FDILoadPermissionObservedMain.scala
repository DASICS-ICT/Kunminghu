// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import freechips.rocketchip.diplomacy.DisableMonitors
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator, SimTop}
import utility.{ChiselDB, Constantin, FileRegisters}
import xiangshan.{DebugOptionsKey, XSTileKey}
import xiangshan.backend.BackendInlinedImp
import xiangshan.backend.datapath.{BypassNetwork, WbDataPath}

// Exports only existing production signals, without changing readiness or state.
class FDILoadPermissionObservedSimTop(implicit p: Parameters) extends SimTop {
  override def desiredName: String = "SimTop"

  override protected def addDifftestObservations(): Unit = {
    val core = l_soc.core_with_l2.head.core
    val memory = core.memBlock.inner.module
    val backend = core.backend.inner.module
    val enabled = core.coreParams.HasFDI
    // Export leaves as passive outputs; nested handshakes retain mixed directions.
    def tap(name: String, source: Data): Unit = source match {
      case record: Record => record.elements.foreach { case (field, value) => tap(s"${name}_$field", value) }
      case vector: Vec[_] => vector.zipWithIndex.foreach { case (value, index) => tap(s"${name}_$index", value) }
      case leaf =>
        val output = IO(Output(chiselTypeOf(leaf))).suggestName(s"l01_observed_$name")
        output := observe(leaf)
    }
    val feature = IO(Output(Bool())).suggestName("l01_observed_feature_present")
    feature := enabled.B
    tap("source_imode", memory.tlbcsr.priv.imode)
    tap("source_v", memory.csrCtrl.virtMode)
    tap("data_dmode", memory.tlbcsr.priv.dmode)
    tap("data_dvirt", memory.tlbcsr.priv.virt)
    require(memory.fdiConfig.isDefined == enabled)
    memory.fdiConfig.foreach(config => tap("config", config))
    memory.fdiMirror.foreach { mirror =>
      tap("mirror_input", mirror.io.distribute)
      tap("mirror_state", mirror.io.state)
    }
    memory.loadUnits.zipWithIndex.foreach { case (load, lane) =>
      val prefix = s"load_$lane"
      require(load.io.fdiConfig.isDefined == enabled && load.fdiPermission.isDefined == enabled)
      tap(s"${prefix}_input", load.io.ldin)
      tap(s"${prefix}_s0_fire", load.s0_fire)
      tap(s"${prefix}_s1_fire", load.s1_fire)
      tap(s"${prefix}_s2_fire", load.s2_fire)
      tap(s"${prefix}_s2_rob", load.s2_in.uop.robIdx)
      tap(s"${prefix}_s2_fullva", load.s2_in.fullva)
      tap(s"${prefix}_s2_tlb_miss", load.s2_in.tlbMiss)
      tap(s"${prefix}_s3_rob", load.s3_in.uop.robIdx)
      tap(s"${prefix}_tlb", load.io.tlb)
      tap(s"${prefix}_ld_cancel", load.io.ldCancel)
      tap(s"${prefix}_fast_uop", load.io.fast_uop)
      tap(s"${prefix}_l2l", load.io.l2l_fwd_out)
      tap(s"${prefix}_cache_s2_kill", load.io.dcache.s2_kill)
      tap(s"${prefix}_queue_output", load.io.lsq.ldin)
      tap(s"${prefix}_split", load.io.misalign_enq)
      tap(s"${prefix}_completion", load.io.ldout)
      tap(s"${prefix}_replay_input", load.io.replay)
      load.io.fdiConfig.foreach(value => tap(s"${prefix}_config", value))
      load.fdiPermission.foreach { checker =>
        tap(s"${prefix}_permission_request", checker.io.req)
        tap(s"${prefix}_permission_response", checker.io.resp)
        tap(s"${prefix}_permission_flush", checker.io.flush)
        tap(s"${prefix}_s2_blocked", load.s2_fdiBlocked)
        tap(s"${prefix}_translation_retry", load.s2_fdiTranslationRetry)
        tap(s"${prefix}_terminal", load.s2_fdiComplete)
        tap(s"${prefix}_s3_blocked", load.s3_fdiBlocked)
        tap(s"${prefix}_error_eligible", load.s3_error_eligible)
      }
    }
    val replay = memory.lsq.loadQueue.loadQueueReplay
    require(replay.fdiFullva.isDefined == enabled && replay.fdiSourcePrivilege.isDefined == enabled &&
      replay.fdiSourceVirtual.isDefined == enabled)
    replay.fdiFullva.foreach(value => tap("replay_fullva", value))
    replay.fdiSourcePrivilege.foreach(value => tap("replay_privilege", value))
    replay.fdiSourceVirtual.foreach(value => tap("replay_virtual", value))
    tap("replay_input", replay.io.enq)
    tap("replay_output", replay.io.replay)
    tap("uncache_request", memory.lsq.loadQueue.io.uncache.req)

    // Approved read-only access to the two exact existing private children.
    val bypassFields = classOf[BackendInlinedImp].getDeclaredFields.filter(field =>
      Set("bypassNetwork", "xiangshan$backend$BackendInlinedImp$$bypassNetwork").contains(field.getName))
    require(bypassFields.length == 1 && classOf[BypassNetwork].isAssignableFrom(bypassFields.head.getType))
    bypassFields.head.setAccessible(true)
    val bypass = bypassFields.head.get(backend).asInstanceOf[BypassNetwork]
    require(bypass eq bypassFields.head.get(backend))
    val wbFields = classOf[BackendInlinedImp].getDeclaredFields.filter(field =>
      Set("wbDataPath", "xiangshan$backend$BackendInlinedImp$$wbDataPath").contains(field.getName))
    require(wbFields.length == 1 && classOf[WbDataPath].isAssignableFrom(wbFields.head.getType))
    wbFields.head.setAccessible(true)
    val wb = wbFields.head.get(backend).asInstanceOf[WbDataPath]
    require(wb eq wbFields.head.get(backend))
    tap("backend_memory_input", backend.io.mem.writebackLda)
    tap("backend_bypass_data", bypass.io.fromExus.mem)
    tap("backend_precise_writeback", wb.io.fromMemExu)
    tap("backend_control_writeback", wb.io.toCtrlBlock.writeback)
  }
}

object FDILoadPermissionObservedMain extends App {
  val (config, firrtlOptions, firtoolOptions) = ArgParser.parse(args)
  require(config(XSTileKey).size == 1)
  require(config(XSTileKey).head.HasVPU && config(XSTileKey).head.VLEN == 128)
  val debug = config(DebugOptionsKey)
  require(debug.FPGAPlatform && !debug.EnableDifftest && !debug.AlwaysBasicDiff)
  Constantin.init(debug.EnableConstantin && !debug.FPGAPlatform)
  ChiselDB.init(debug.EnableChiselDB && !debug.FPGAPlatform)
  Generator.execute(firrtlOptions,
    DisableMonitors(p => new FDILoadPermissionObservedSimTop()(p))(config), firtoolOptions)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
