// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import freechips.rocketchip.diplomacy.DisableMonitors
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, FileRegisters}
import xiangshan.{DebugOptionsKey, XSTileKey}

/** Keeps the existing reference bridge and adds source observations only for this emitter. */
class UserTimerCycleSimTop(implicit p: Parameters) extends UserTimerReferenceSimTop()(p) {
  override protected def addDifftestObservations(): Unit = {
    super.addDifftestObservations()
    UserTimerCycleObservations.attach(this)
  }
}

object UserTimerCycleMain extends App {
  require(sys.env.get("UIT07_PROTOCOL_VERSION").contains("1"), "UIT07_PROTOCOL_VERSION must be 1")
  val enabled = sys.env.get("UIT07_USER_TIMER") match {
    case Some("1") => true
    case Some("0") => false
    case _ => throw new IllegalArgumentException("UIT07_USER_TIMER must be 0 or 1")
  }
  val (base, firrtlOpts, firtoolOpts) = top.ArgParser.parse(args)
  val config = base.alterPartial {
    case XSTileKey => base(XSTileKey).map(_.copy(HasUserTimerInterrupt = enabled))
  }
  require(config(XSTileKey).size == 1)
  val options = config(DebugOptionsKey)
  require(!options.FPGAPlatform && options.EnableDifftest)
  Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
  ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
  top.Generator.execute(firrtlOpts, DisableMonitors(p =>
    if (enabled) new UserTimerCycleSimTop()(p) else new UserTimerReferenceDisabledSimTop()(p))(config), firtoolOpts)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
