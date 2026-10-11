// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.MemExuOutput
import xiangshan.mem.Bundles._

// Reuse the production STA/STD, TLB/PMP, SQ, Sbuffer and SMB connections.
// These additional ports observe ownership and publication; they do not steer the modules.
class FDIStoreMisalignHarness(implicit p: Parameters) extends FDIStorePermissionHarness {
  val view = IO(Output(new Bundle {
    val splitFull = Bool()
    val splitState = UInt(4.W)
    val splitOwner = chiselTypeOf(split.io.splitStoreReq.bits.uop.robIdx)
    val splitSq = chiselTypeOf(split.io.splitStoreReq.bits.uop.sqIdx)
    val splitOriginalAddress = UInt(XLEN.W)
    val splitResponse = Valid(new SqWriteBundle)
    val splitWriteback = Valid(new MemExuOutput)
    val splitRevoke = Vec(stores.size, Bool())
    val splitEnqueue = Vec(stores.size, Valid(new LsPipelineBundle))
    val crossPageCanDequeue = Bool()
    val crossPageOwner = Bool()
    val splitDequeue = Bool()
    val exceptionValid = Bool()
    val exceptionOwner = chiselTypeOf(split.io.splitStoreReq.bits.uop.robIdx)
    val exceptionAddress = UInt(XLEN.W)
    val exceptionNeedsExtension = Bool()
    val exceptionIsHyper = Bool()
  }))

  view.splitFull := split.io.full
  view.splitState := observe(split.bufferState)
  view.splitOwner := observe(split.req.uop.robIdx)
  view.splitSq := observe(split.req.uop.sqIdx)
  view.splitOriginalAddress := observe(split.req.fullva)
  view.splitResponse := split.io.splitStoreResp
  view.splitWriteback.valid := split.io.writeBack.fire
  view.splitWriteback.bits := split.io.writeBack.bits
  stores.zipWithIndex.foreach { case (store, lane) =>
    view.splitRevoke(lane) := store.io.misalign_enq.revoke
    view.splitEnqueue(lane).valid := store.io.misalign_enq.req.fire
    view.splitEnqueue(lane).bits := store.io.misalign_enq.req.bits
  }
  view.crossPageCanDequeue := split.io.sqControl.toStoreQueue.crossPageCanDeq
  view.crossPageOwner := split.io.sqControl.toStoreQueue.crossPageWithHit
  view.splitDequeue := split.io.sqControl.toStoreMisalignBuffer.doDeq
  view.exceptionValid := observe(queue.exceptionBuffer.req_valid)
  view.exceptionOwner := observe(queue.exceptionBuffer.req.uop.robIdx)
  view.exceptionAddress := Mux(split.io.overwriteExpBuf.valid,
    split.io.overwriteExpBuf.vaddr, io.exceptionAddress)
  // Match MemBlock's scalar exception selection before its GenExceptionVa conversion.
  view.exceptionNeedsExtension := split.io.overwriteExpBuf.valid || queue.io.exceptionAddr.vaNeedExt
  view.exceptionIsHyper := Mux(split.io.overwriteExpBuf.valid,
    split.io.overwriteExpBuf.isHyper, queue.io.exceptionAddr.isHyper)
}
