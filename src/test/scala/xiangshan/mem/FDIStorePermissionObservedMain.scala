// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import freechips.rocketchip.diplomacy.DisableMonitors
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator, SimTop}
import utility.{ChiselDB, Constantin, FileRegisters}
import xiangshan.{DebugOptionsKey, XSTileKey}

// These ports keep production signals observable without driving functional inputs.
class FDIStorePermissionObservedSimTop(implicit p: Parameters) extends SimTop {
  override def desiredName: String = "SimTop"

  override protected def addDifftestObservations(): Unit = {
    val core = l_soc.core_with_l2.head.core
    val memory = core.memBlock.inner.module
    val backend = core.backend.inner.module
    val control = core.backend.inner.ctrlBlock.module
    val queue = memory.lsq.storeQueue
    val buffer = memory.sbuffer
    val enabled = core.coreParams.HasFDI
    require(memory.storeUnits.size == 2 && memory.stdExeUnits.size == 2)
    require(memory.hybridUnits.isEmpty && buffer.io.in.size == 2)
    // Export leaves as passive outputs; nested handshakes retain mixed directions.
    def tap(name: String, source: Data): Unit = source match {
      case record: Record => record.elements.foreach { case (field, value) => tap(s"${name}_$field", value) }
      case vector: Vec[_] => vector.zipWithIndex.foreach { case (value, index) => tap(s"${name}_$index", value) }
      case leaf =>
        val output = IO(Output(chiselTypeOf(leaf))).suggestName(s"l03_observed_$name")
        output := observe(leaf)
    }
    val feature = IO(Output(Bool())).suggestName("l03_observed_feature_present")
    feature := enabled.B
    tap("source_imode", memory.tlbcsr.priv.imode)
    tap("source_v", memory.csrCtrl.virtMode)
    tap("data_dmode", memory.tlbcsr.priv.dmode)
    tap("data_dvirt", memory.tlbcsr.priv.virt)
    tap("distribution_input", memory.io.ooo_to_mem.csrCtrl.distribute_csr)
    tap("distribution_delayed", memory.csrCtrl.distribute_csr)
    require(memory.fdiConfig.isDefined == enabled && memory.fdiMirror.isDefined == enabled)
    memory.fdiConfig.foreach(value => tap("config", value))
    memory.fdiMirror.foreach { mirror =>
      tap("mirror_input", mirror.io.distribute)
      tap("mirror_state", mirror.io.state)
    }
    memory.storeUnits.zipWithIndex.foreach { case (store, lane) =>
      val prefix = s"store_$lane"
      require(store.io.fdiConfig.isDefined == enabled && store.fdiPermission.isDefined == enabled)
      tap(s"${prefix}_input", store.io.stin)
      tap(s"${prefix}_s0_fire", store.s0_fire)
      tap(s"${prefix}_s1_fire", store.s1_fire)
      tap(s"${prefix}_s2_fire", store.s2_fire)
      tap(s"${prefix}_s1_owner", store.s1_out.uop.robIdx)
      tap(s"${prefix}_s1_fullva", store.s1_out.fullva)
      tap(s"${prefix}_s2_owner", store.s2_out.uop.robIdx)
      tap(s"${prefix}_s2_exceptions", store.s2_out.uop.exceptionVec)
      tap(s"${prefix}_tlb", store.io.tlb)
      tap(s"${prefix}_pmp", store.io.pmp)
      tap(s"${prefix}_queue_primary", store.io.lsq)
      tap(s"${prefix}_queue_supplement", store.io.lsq_replenish)
      tap(s"${prefix}_completion", store.io.stout)
      tap(s"${prefix}_split_admission", store.io.misalign_enq)
      store.io.fdiConfig.foreach(value => tap(s"${prefix}_config", value))
      store.s1_out.fdiSourcePrivilege.foreach(value => tap(s"${prefix}_source_privilege", value))
      store.s1_out.fdiSourceVirtual.foreach(value => tap(s"${prefix}_source_virtual", value))
      store.s1_out.uop.fdiNotTrusted.foreach(value => tap(s"${prefix}_trust", value))
      store.s2_out.uop.fdiException.foreach(value => tap(s"${prefix}_exception_record", value))
      store.fdiPermission.foreach { checker =>
        tap(s"${prefix}_permission_request", checker.io.req)
        tap(s"${prefix}_permission_response", checker.io.resp)
        tap(s"${prefix}_permission_flush", checker.io.flush)
        tap(s"${prefix}_blocked", store.s2_fdiBlocked)
      }
    }
    tap("queue_primary", queue.io.storeAddrIn)
    tap("queue_supplement", queue.io.storeAddrInRe)
    tap("queue_allocated", queue.allocated)
    tap("queue_wait_s2", queue.waitStoreS2)
    tap("queue_has_exception", queue.hasException)
    tap("queue_nc", queue.nc)
    tap("queue_mmio", queue.mmio)
    tap("queue_uncache", queue.io.uncache.req)
    tap("queue_data_buffer_input", queue.dataBuffer.io.enq)
    tap("queue_data_buffer_output", queue.dataBuffer.io.deq)
    queue.fdiSlotReallocated.foreach(value => tap("queue_reallocation", value))
    for (lane <- buffer.io.in.indices) {
      tap(s"buffer_${lane}_input", buffer.io.in(lane))
      tap(s"buffer_${lane}_write", buffer.writeReq(lane))
    }
    tap("buffer_state", buffer.stateVec)
    tap("buffer_mask", buffer.mask)
    tap("buffer_cache_write", buffer.io.dcache.req)
    tap("split_request", memory.storeMisalignBuffer.io.splitStoreReq)
    tap("split_completion", memory.storeMisalignBuffer.io.writeBack)
    tap("backend_store_input", backend.io.mem.writebackSta)
    tap("backend_exception_address", backend.io.mem.exceptionAddr)
    tap("precise_exception", control.io.robio.exception)
  }
}

object FDIStorePermissionObservedMain extends App {
  val (config, firrtlOptions, firtoolOptions) = ArgParser.parse(args)
  require(config(XSTileKey).size == 1)
  require(config(XSTileKey).head.HasVPU && config(XSTileKey).head.VLEN == 128)
  val debug = config(DebugOptionsKey)
  require(debug.FPGAPlatform && !debug.EnableDifftest && !debug.AlwaysBasicDiff)
  Constantin.init(debug.EnableConstantin && !debug.FPGAPlatform)
  ChiselDB.init(debug.EnableChiselDB && !debug.FPGAPlatform)
  Generator.execute(firrtlOptions,
    DisableMonitors(p => new FDIStorePermissionObservedSimTop()(p))(config), firtoolOptions)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
