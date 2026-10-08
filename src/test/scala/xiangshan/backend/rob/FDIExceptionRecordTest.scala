// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.rob

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.wrapper.JumpUnit
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

class FDIExceptionRecordTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "DASICS exception transport and oldest selection"

  private def parameters(enabled: Boolean): Parameters = {
    val (base, _, _) = top.ArgParser.parse(Array(
      "--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768",
      "--fpga-platform", "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    base.alterPartial { case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled) }
  }

  it should "retain source-specific payloads through production FU filters and ROB stages" in {
    val root = Paths.get(sys.props.getOrElse("e01.runRoot", throw new IllegalArgumentException("Set e01.runRoot")))
    require(root.isAbsolute && Paths.get("").toRealPath() == root.toRealPath())
    implicit val p: Parameters = parameters(true)
    val core = p(XSCoreParamsKey)
    val exus = core.backendParams.allExuParams.filter(_.exceptionOut.nonEmpty)
    val configs = (FuConfig.allConfigs ++ Seq(FuConfig.FakeHystaCfg) ++ exus.flatMap(_.fuConfigs)).distinct
    // The allowed producer inventory comes from the task contract, not the
    // exception masks being checked in the production FU configurations.
    val permittedNames = Set("jmp", "brh", "csr", "ldu", "sta", "hylda", "hysta", "mou",
      "vldu", "vstu", "vsegldu", "vsegstu")
    val ports = exus.indices.filter(i => exus(i).fuConfigs.exists(f => permittedNames(f.name)))
    require(ports.nonEmpty)
    val csr = exus.indexWhere(_.fuConfigs.exists(_.name == "csr"))
    val jump = exus.indexWhere(_.fuConfigs.exists(_.name == "jmp"))
    val branch = exus.indexWhere(_.fuConfigs.exists(_.name == "brh"))
    require(csr >= 0 && jump >= 0 && branch >= 0)
    val standardPriority = Seq(16, 3, 12, 20, 1, 2, 22, 0, 11, 9, 10, 8,
      6, 4, 15, 13, 23, 21, 7, 5, 19)
    require(ExceptionNO.priorities == standardPriority ++ Seq(24, 25))
    var assertions = 0
    var cycles = 0
    var transactions = 0
    test(new FDIExceptionInjectionHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("rtl-on"))) { dut =>
      def step(n: Int = 1): Unit = { dut.clock.step(n); cycles += n }
      def empty(x: FDIExceptionInjection): Unit = {
        x.robIdx.flag.poke(false.B); x.robIdx.value.poke(0.U)
        x.standard.poke(0.U); x.denied.poke(false.B)
        x.privilege.poke(0.U); x.virtual.poke(false.B)
        x.tval.poke(0.U); x.reason.poke(0.U)
        x.replay.poke(false.B); x.flushPipe.poke(false.B)
        x.vector.poke(false.B); x.vstart.poke(0.U); x.vuopIdx.poke(0.U)
      }
      def idle(): Unit = {
        dut.io.wb.foreach { x => x.valid.poke(false.B); empty(x.bits) }
        dut.io.enq.foreach { x => x.valid.poke(false.B); empty(x.bits) }
        dut.io.redirect.valid.poke(false.B)
        dut.io.redirect.bits.robIdx.flag.poke(false.B)
        dut.io.redirect.bits.robIdx.value.poke(0.U)
        dut.io.redirect.bits.flushItself.poke(false.B)
        dut.io.flush.poke(false.B)
        dut.io.filterInput.poke(0.U)
      }
      def restart(): Unit = {
        idle(); dut.reset.poke(true.B); step(2); dut.reset.poke(false.B); step(2)
        dut.io.state.valid.expect(false.B); assertions += 1
      }
      def fault(port: Int, idx: Int, address: BigInt, reason: Int = 2,
                privilege: Int = 0, flag: Boolean = false): Unit = {
        val in = dut.io.wb(port)
        in.valid.poke(true.B); in.bits.robIdx.value.poke(idx.U)
        in.bits.robIdx.flag.poke(flag.B)
        in.bits.denied.poke(true.B); in.bits.privilege.poke(privilege.U)
        in.bits.reason.poke(reason.U); in.bits.tval.poke(address.U)
        transactions += 1
      }
      def finishPulse(): Unit = { step(); idle(); step(2) }
      def expect(idx: Int, address: BigInt, reason: Int, cause: Int,
                 flag: Boolean = false): Unit = {
        dut.io.state.valid.expect(true.B)
        dut.io.state.bits.robIdx.value.expect(idx.U)
        dut.io.state.bits.robIdx.flag.expect(flag.B)
        dut.io.state.bits.exceptionVec(cause).expect(true.B)
        dut.io.state.bits.exceptionVec(if (cause == 24) 25 else 24).expect(false.B)
        dut.io.state.bits.fdiException.get.tval.expect(address.U)
        dut.io.state.bits.fdiException.get.reason.expect(reason.U)
        dut.io.selectedCause.expect(cause.U)
        assertions += 8
      }

      restart()
      val dasicsMask = (BigInt(1) << 24) | (BigInt(1) << 25)
      dut.io.filterInput.poke(dasicsMask.U)
      configs.zipWithIndex.foreach { case (config, index) =>
        dut.io.perFuMasks(index).expect((if (permittedNames(config.name)) dasicsMask else BigInt(0)).U)
        assertions += 1
      }
      val legacyByName = Map(
        "jmp" -> Set.empty[Int], "brh" -> Set.empty[Int],
        "csr" -> Set(2, 22, 3, 8, 9, 10, 11),
        "ldu" -> Set(2, 4, 5, 13, 21, 3, 19), "sta" -> Set(2, 6, 7, 15, 23, 3),
        "hylda" -> Set(4, 5, 13, 21), "hysta" -> Set(6, 7, 15, 23),
        "mou" -> Set(4, 5, 13, 21, 3, 19, 6, 7, 15, 23),
        "vldu" -> Set(4, 5, 13, 21, 3), "vstu" -> Set(6, 7, 15, 23, 3),
        "vsegldu" -> Set(4, 5, 13, 3), "vsegstu" -> Set(6, 7, 15, 3))
      dut.io.filterInput.poke(((BigInt(1) << 26) - 1).U)
      configs.zipWithIndex.foreach { case (config, index) =>
        legacyByName.get(config.name).foreach { legacy =>
          val expected = (legacy ++ Set(24, 25)).foldLeft(BigInt(0))((mask, bit) => mask.setBit(bit))
          dut.io.perFuMasks(index).expect(expected.U)
          assertions += 1
        }
      }

      for (port <- ports; mode <- Seq(0, 1)) {
        restart()
        val address = (BigInt(1) << 63) + BigInt(port * 64 + mode * 2 + 1)
        fault(port, 12, address, reason = 4, privilege = mode)
        dut.io.acceptedMasks(port).expect((BigInt(1) << (24 + mode)).U)
        assertions += 1
        finishPulse(); expect(12, address, 4, 24 + mode)
      }

      for (privilege <- 0 to 3; virtual <- Seq(false, true)
           if virtual || privilege >= 2) {
        restart(); fault(csr, 8, 0, reason = 1, privilege = privilege)
        dut.io.wb(csr).bits.virtual.poke(virtual.B)
        finishPulse(); dut.io.state.valid.expect(false.B); assertions += 1
      }

      restart(); fault(csr, 8, 0, reason = 1)
      dut.io.wb(csr).bits.denied.poke(false.B)
      finishPulse(); dut.io.state.valid.expect(false.B); assertions += 1

      val storePort = exus.indexWhere(_.fuConfigs.exists(_.name == "sta"))
      require(storePort >= 0)
      restart(); fault(storePort, 8, BigInt("8000000000000103", 16), reason = 3)
      finishPulse(); expect(8, BigInt("8000000000000103", 16), 3, 24)

      restart(); fault(csr, 30, 0, reason = 1)
      dut.io.enq(0).valid.poke(true.B)
      dut.io.enq(0).bits.robIdx.value.poke(40.U)
      dut.io.enq(0).bits.standard.poke((BigInt(1) << 12).U)
      finishPulse(); expect(30, 0, 1, 24)

      restart(); fault(jump, 40, BigInt("8000000000001001", 16), reason = 4)
      fault(csr, 12, 0, reason = 1, privilege = 1)
      finishPulse(); expect(12, 0, 1, 25)
      fault(branch, 7, BigInt("ffffffffffff8003", 16), reason = 4)
      finishPulse(); expect(7, BigInt("ffffffffffff8003", 16), 4, 24)

      restart(); fault(jump, core.RobSize - 2, 0x101, reason = 4)
      fault(csr, 3, 0, reason = 1, privilege = 1, flag = true)
      finishPulse(); expect(core.RobSize - 2, 0x101, 4, 24)

      for (cancelStage <- 0 to 3) {
        restart(); fault(jump, 20, 0x333, reason = 4)
        if (cancelStage > 0) { step(); idle(); if (cancelStage > 1) step(cancelStage - 1) }
        dut.io.redirect.valid.poke(true.B)
        dut.io.redirect.bits.robIdx.value.poke(20.U)
        dut.io.redirect.bits.flushItself.poke(true.B)
        step(); idle(); step(4)
        dut.io.state.valid.expect(false.B); assertions += 1
      }

      restart(); fault(jump, 10, 0x111, reason = 4); finishPulse()
      fault(csr, 20, 0, reason = 1)
      dut.io.redirect.valid.poke(true.B)
      dut.io.redirect.bits.robIdx.value.poke(15.U)
      finishPulse(); expect(10, 0x111, 4, 24)

      restart(); fault(csr, 18, 0, reason = 1)
      dut.io.wb(csr).bits.standard.poke((BigInt(1) << 2).U)
      finishPulse()
      dut.io.state.bits.exceptionVec(24).expect(true.B)
      dut.io.state.bits.exceptionVec(2).expect(true.B)
      dut.io.selectedCause.expect(2.U); assertions += 3

      val vectorPort = exus.indexWhere(_.fuConfigs.exists(_.name == "vldu"))
      require(vectorPort >= 0)
      restart(); fault(vectorPort, 22, 0x801, reason = 2)
      dut.io.wb(vectorPort).bits.vector.poke(true.B)
      dut.io.wb(vectorPort).bits.vstart.poke(8.U)
      dut.io.wb(vectorPort).bits.vuopIdx.poke(8.U)
      finishPulse(); expect(22, 0x801, 2, 24)
      fault(vectorPort, 22, 0x401, reason = 2)
      dut.io.wb(vectorPort).bits.vector.poke(true.B)
      dut.io.wb(vectorPort).bits.vstart.poke(4.U)
      dut.io.wb(vectorPort).bits.vuopIdx.poke(4.U)
      finishPulse(); expect(22, 0x401, 2, 24)
      dut.io.state.bits.vstart.expect(4.U); assertions += 1
      // The selected state must identify the uop that supplied its payload.
      dut.io.state.bits.vuopIdx.expect(4.U); assertions += 1
      fault(vectorPort, 22, 0xc01, reason = 2)
      dut.io.wb(vectorPort).bits.vector.poke(true.B)
      dut.io.wb(vectorPort).bits.vstart.poke(12.U)
      dut.io.wb(vectorPort).bits.vuopIdx.poke(12.U)
      finishPulse(); expect(22, 0x401, 2, 24)
      dut.io.state.bits.vuopIdx.expect(4.U); assertions += 1

      restart()
      dut.io.wb(csr).valid.poke(true.B)
      dut.io.wb(csr).bits.robIdx.value.poke(14.U)
      dut.io.wb(csr).bits.replay.poke(true.B)
      finishPulse()
      dut.io.state.valid.expect(true.B)
      dut.io.state.bits.exceptionVec.foreach(_.expect(false.B))
      dut.io.state.bits.replayInst.expect(true.B); assertions += 3
      dut.io.flush.poke(true.B); step(); idle(); step(4)
      dut.io.state.valid.expect(false.B); assertions += 1
    }
    Files.write(root.resolve("on-summary.json"),
      (s"""{"assertions":$assertions,"cycles":$cycles,"transactions":$transactions,"injected_ports":${ports.size}}""" + "\n")
        .getBytes(StandardCharsets.UTF_8))
  }

  it should "omit the dedicated payload while retaining ordinary exceptions when disabled" in {
    val root = Paths.get(sys.props.getOrElse("e01.runRoot", throw new IllegalArgumentException("Set e01.runRoot")))
    require(root.isAbsolute && Paths.get("").toRealPath() == root.toRealPath())
    implicit val p: Parameters = parameters(false)
    val exus = p(XSCoreParamsKey).backendParams.allExuParams.filter(_.exceptionOut.nonEmpty)
    val csr = exus.indexWhere(_.fuConfigs.exists(_.name == "csr"))
    require(csr >= 0)
    test(new FDIExceptionInjectionHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("rtl-off"))) { dut =>
      require(dut.io.state.bits.fdiException.isEmpty && dut.io.out.bits.fdiException.isEmpty)
      require(dut.io.state.bits.exceptionVec.size == 26)
      (dut.io.wb ++ dut.io.enq).foreach { input =>
        input.valid.poke(false.B)
        input.bits.robIdx.flag.poke(false.B); input.bits.robIdx.value.poke(0.U)
        input.bits.standard.poke(0.U); input.bits.denied.poke(false.B)
        input.bits.privilege.poke(0.U); input.bits.virtual.poke(false.B)
        input.bits.tval.poke(0.U); input.bits.reason.poke(0.U)
        input.bits.replay.poke(false.B); input.bits.flushPipe.poke(false.B)
        input.bits.vector.poke(false.B); input.bits.vstart.poke(0.U); input.bits.vuopIdx.poke(0.U)
      }
      dut.io.redirect.valid.poke(false.B)
      dut.io.redirect.bits.robIdx.flag.poke(false.B)
      dut.io.redirect.bits.robIdx.value.poke(0.U)
      dut.io.redirect.bits.flushItself.poke(false.B)
      dut.io.flush.poke(false.B)
      dut.io.filterInput.poke(0.U)
      dut.reset.poke(true.B); dut.clock.step(2)
      dut.reset.poke(false.B); dut.clock.step(2)
      dut.io.wb(csr).valid.poke(true.B)
      dut.io.wb(csr).bits.robIdx.value.poke(11.U)
      dut.io.wb(csr).bits.standard.poke((BigInt(1) << 2).U)
      dut.clock.step(); dut.io.wb(csr).valid.poke(false.B); dut.clock.step(2)
      dut.io.state.valid.expect(true.B)
      dut.io.state.bits.robIdx.value.expect(11.U)
      dut.io.state.bits.exceptionVec(2).expect(true.B)
      dut.io.state.bits.exceptionVec(24).expect(false.B)
      dut.io.state.bits.exceptionVec(25).expect(false.B)
      dut.io.selectedCause.expect(2.U)
    }
    Files.write(root.resolve("off-summary.json"),
      "{\"dedicated_payload\":false,\"exception_bits\":26,\"ordinary_cause\":2}\n".getBytes(StandardCharsets.UTF_8))
  }

  for (enabled <- Seq(false, true)) {
    it should s"elaborate actual scalar Jump with exception outputs and HasFDI=$enabled" in {
      val root = Paths.get(sys.props.getOrElse("e01.runRoot", throw new IllegalArgumentException("Set e01.runRoot")))
      require(root.isAbsolute && Paths.get("").toRealPath() == root.toRealPath())
      implicit val p: Parameters = parameters(enabled)
      test(new JumpUnit(FuConfig.JmpCfg)).withAnnotations(Seq(
        VerilatorBackendAnnotation, TargetDirAnnotation(s"rtl-jump-${if (enabled) "on" else "off"}"))) { dut =>
        def clear(data: Data): Unit = data match {
          case record: Record => record.elements.values.foreach(clear)
          case vector: Vec[_] => vector.foreach(clear)
          case value: Bool => value.poke(false.B)
          case value: UInt => value.poke(0.U)
          case value: SInt => value.poke(0.S)
          case other => throw new IllegalArgumentException(s"Unsupported test input ${other.getClass.getName}")
        }
        clear(dut.io.in.bits)
        clear(dut.io.flush.bits)
        dut.io.instrAddrTransType.foreach(clear)
        dut.io.in.valid.poke(false.B)
        dut.io.out.ready.poke(true.B)
        dut.io.flush.valid.poke(false.B)
        dut.reset.poke(true.B); dut.clock.step(2)
        dut.reset.poke(false.B)
        dut.io.in.bits.data.pc.get.poke(BigInt("80000000", 16).U)
        dut.io.in.bits.data.imm.poke(8.U)
        dut.io.in.bits.data.nextPcOffset.get.poke(2.U)
        dut.io.in.bits.ctrl.fuOpType.poke(0.U)
        dut.io.in.valid.poke(true.B)
        dut.clock.step()
        dut.io.out.valid.expect(true.B)
        dut.io.out.bits.res.data.expect(BigInt("80000004", 16).U)
        dut.io.out.bits.res.redirect.get.bits.fullTarget.expect(BigInt("80000008", 16).U)
        dut.io.out.bits.ctrl.exceptionVec.get.foreach(_.expect(false.B))
        require(dut.io.out.bits.ctrl.fdiException.isDefined == enabled)
        dut.io.out.bits.ctrl.fdiException.foreach { record =>
          record.tval.expect(0.U)
          record.reason.expect(0.U)
        }
      }
      Files.write(root.resolve(s"jump-${if (enabled) "on" else "off"}-summary.json"),
        s"""{"has_fdi":$enabled,"production_jump":true,"ordinary_target_link_checked":true}""".getBytes(StandardCharsets.UTF_8))
    }
  }

  for ((name, enabled, extraMask, reason, address) <- Seq(
    ("dual-origin", true, BigInt(1) << 25, 2, BigInt(1)),
    ("zero-reason", true, BigInt(0), 0, BigInt(1)),
    ("reserved-reason", true, BigInt(0), 5, BigInt(1)),
    ("ecall-address", true, BigInt(0), 1, BigInt(1)),
    ("disabled-fault", false, BigInt(0), 2, BigInt(1)))) {
    it should s"reject an invalid $name exception candidate" in {
      val root = Paths.get(sys.props.getOrElse("e01.runRoot", throw new IllegalArgumentException("Set e01.runRoot")))
      require(root.isAbsolute && Paths.get("").toRealPath() == root.toRealPath())
      implicit val p: Parameters = parameters(enabled)
      val exus = p(XSCoreParamsKey).backendParams.allExuParams.filter(_.exceptionOut.nonEmpty)
      val csr = exus.indexWhere(_.fuConfigs.exists(_.name == "csr"))
      require(csr >= 0)
      assertThrows[ChiselAssertionError] {
        test(new FDIExceptionInjectionHarness).withAnnotations(Seq(
          VerilatorBackendAnnotation, TargetDirAnnotation(s"rtl-invalid-$name"))) { dut =>
          (dut.io.wb ++ dut.io.enq).foreach { input =>
            input.valid.poke(false.B)
            input.bits.robIdx.flag.poke(false.B); input.bits.robIdx.value.poke(0.U)
            input.bits.standard.poke(0.U); input.bits.denied.poke(false.B)
            input.bits.privilege.poke(0.U); input.bits.virtual.poke(false.B)
            input.bits.tval.poke(0.U); input.bits.reason.poke(0.U)
            input.bits.replay.poke(false.B); input.bits.flushPipe.poke(false.B)
            input.bits.vector.poke(false.B); input.bits.vstart.poke(0.U); input.bits.vuopIdx.poke(0.U)
          }
          dut.io.redirect.valid.poke(false.B)
          dut.io.redirect.bits.robIdx.flag.poke(false.B)
          dut.io.redirect.bits.robIdx.value.poke(0.U)
          dut.io.redirect.bits.flushItself.poke(false.B)
          dut.io.flush.poke(false.B); dut.io.filterInput.poke(0.U)
          dut.reset.poke(true.B); dut.clock.step(2)
          dut.reset.poke(false.B); dut.clock.step(2)
          dut.io.wb(csr).valid.poke(true.B)
          dut.io.wb(csr).bits.denied.poke(true.B)
          dut.io.wb(csr).bits.standard.poke(extraMask.U)
          dut.io.wb(csr).bits.reason.poke(reason.U)
          dut.io.wb(csr).bits.tval.poke(address.U)
          dut.clock.step()
        }
      }
      Files.write(root.resolve(s"expected-rejection-$name.json"),
        s"""{"case":"$name","assertion_caught":true}""".getBytes(StandardCharsets.UTF_8))
    }
  }
}
