package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.file.Paths
import scala.util.Random

class UserTimerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "UserTimer"

  it should "expire on the exact eligible tick and preserve event priority" in {
    val runRoot = Paths.get(sys.props.getOrElse(
      "uit01.runRoot",
      throw new IllegalArgumentException("Set uit01.runRoot to an absolute build output directory")
    ))
    require(runRoot.isAbsolute, "uit01.runRoot must be absolute")
    require(Paths.get("").toRealPath() == runRoot.toRealPath(),
      "The test working directory must be uit01.runRoot")
    val maxValue = (BigInt(1) << 64) - 1
    val seed = 0x55495430314cL
    val randomCycles = 4096

    case class Stimulus(
      tick: Boolean = false,
      write: Option[BigInt] = None,
      consume: Boolean = false,
      reset: Boolean = false
    )

    // Expiration is a deadline on an unbounded eligible-tick timeline. The model
    // never decrements a counter or wraps arithmetic to the hardware width.
    class DeadlineModel {
      private var ticks = BigInt(0)
      private var deadline: Option[BigInt] = None
      private var lastConsume: Option[BigInt] = None

      def remaining: BigInt = deadline.map(d => (d - ticks).max(BigInt(0))).getOrElse(BigInt(0))
      def pending: Boolean = deadline.exists { d =>
        ticks >= d && !lastConsume.exists(_ >= d)
      }

      def advance(input: Stimulus): Unit = {
        if (input.reset) {
          ticks = 0
          deadline = None
          lastConsume = None
        } else if (input.write.nonEmpty) {
          deadline = input.write.filter(_ > 0).map(ticks + _)
          lastConsume = None
        } else if (input.consume) {
          lastConsume = Some(ticks)
        } else if (input.tick) {
          ticks += 1
        }
      }
    }

    test(new UserTimer).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      TargetDirAnnotation("rtl-simulation")
    )) { dut =>
      val model = new DeadlineModel
      var cycles = 0
      var groups = 0
      var expirationEdges = 0

      def drive(input: Stimulus): Unit = {
        dut.reset.poke(input.reset.B)
        dut.io.tickEnable.poke(input.tick.B)
        dut.io.write.valid.poke(input.write.nonEmpty.B)
        dut.io.write.bits.poke(input.write.getOrElse(BigInt(0)).U(64.W))
        dut.io.consume.poke(input.consume.B)
      }

      def check(label: String, remaining: BigInt, pending: Boolean): Unit = {
        withClue(s"$label at clock $cycles: ") {
          dut.io.remaining.expect(remaining.U(64.W))
          dut.io.pending.expect(pending.B)
        }
      }

      def advance(label: String, input: Stimulus, expected: Option[(BigInt, Boolean)] = None): Unit = {
        drive(input)
        check(s"$label before edge", model.remaining, model.pending)
        val previouslyPending = model.pending
        dut.clock.step()
        cycles += 1
        model.advance(input)
        if (!previouslyPending && model.pending) expirationEdges += 1
        expected.foreach { case (remaining, pending) =>
          withClue(s"$label directed expectation disagrees with deadline model: ") {
            assert(model.remaining == remaining)
            assert(model.pending == pending)
          }
          check(s"$label directed", remaining, pending)
        }
        check(label, model.remaining, model.pending)
      }

      def cycle(label: String, input: Stimulus, remaining: BigInt, pending: Boolean): Unit =
        advance(label, input, Some((remaining, pending)))

      def group(body: => Unit): Unit = {
        groups += 1
        body
      }

      drive(Stimulus(reset = true))
      dut.clock.step(2)
      cycles += 2
      check("initial reset", 0, pending = false)

      group {
        cycle("zero write wins over consume and tick", Stimulus(tick = true, write = Some(0), consume = true), 0, false)
        for (i <- 1 to 8) cycle(s"inactive tick $i", Stimulus(tick = true), 0, false)
      }

      group {
        cycle("load one does not tick", Stimulus(tick = true, write = Some(1)), 1, false)
        for (i <- 1 to 4) cycle(s"freeze one $i", Stimulus(), 1, false)
        cycle("one expires on its first tick", Stimulus(tick = true), 0, true)
        for (i <- 1 to 8) cycle(s"sticky pending $i", Stimulus(tick = i % 2 == 0), 0, true)
        cycle("consume pending wins over tick", Stimulus(tick = true, consume = true), 0, false)
        for (i <- 1 to 8) cycle(s"no retrigger after consume $i", Stimulus(tick = true), 0, false)
      }

      group {
        cycle("load two", Stimulus(write = Some(2)), 2, false)
        cycle("consume active count wins over tick", Stimulus(tick = true, consume = true), 2, false)
        cycle("first tick of two", Stimulus(tick = true), 1, false)
        cycle("freeze before expiration", Stimulus(), 1, false)
        cycle("consume at one suppresses concurrent tick", Stimulus(tick = true, consume = true), 1, false)
        cycle("second eligible tick expires", Stimulus(tick = true), 0, true)
      }

      group {
        cycle("rearm pending timer", Stimulus(write = Some(7)), 7, false)
        for (i <- 1 to 7) {
          cycle(s"seven freeze before tick $i", Stimulus(), 8 - i, false)
          cycle(s"seven eligible tick $i", Stimulus(tick = true), 7 - i, i == 7)
        }
      }

      group {
        cycle("large exact deadline load", Stimulus(tick = true, write = Some(257)), 257, false)
        for (i <- 1 to 257) {
          if (i % 11 == 0) cycle(s"large deadline pause $i", Stimulus(), 258 - i, false)
          cycle(s"large deadline tick $i", Stimulus(tick = true), 257 - i, i == 257)
        }
      }

      group {
        cycle("reload pending with all events", Stimulus(tick = true, write = Some(2), consume = true), 2, false)
        cycle("approach old expiration", Stimulus(tick = true), 1, false)
        cycle("reload beats old expiration", Stimulus(tick = true, write = Some(3)), 3, false)
        cycle("new deadline tick one", Stimulus(tick = true), 2, false)
        cycle("replace active deadline with one", Stimulus(tick = true, write = Some(1), consume = true), 1, false)
        cycle("replacement expires", Stimulus(tick = true), 0, true)
        cycle("consecutive reload one", Stimulus(write = Some(2)), 2, false)
        cycle("consecutive reload two", Stimulus(write = Some(1)), 1, false)
        cycle("consecutive reload expires", Stimulus(tick = true), 0, true)
      }

      group {
        cycle("cancel pending", Stimulus(write = Some(0)), 0, false)
        cycle("load before cancel", Stimulus(write = Some(1)), 1, false)
        cycle("cancel beats expiration and consume", Stimulus(tick = true, write = Some(0), consume = true), 0, false)
        for (i <- 1 to 4) cycle(s"cancel stays idle $i", Stimulus(tick = true), 0, false)
        cycle("consume empty timer", Stimulus(consume = true), 0, false)
      }

      group {
        for (value <- Seq(maxValue, maxValue - 1, BigInt(1) << 63, (BigInt(1) << 32) + 1)) {
          cycle(s"full width load $value", Stimulus(tick = true, write = Some(value)), value, false)
          for (i <- 1 to 5) cycle(s"full width $value tick $i", Stimulus(tick = true), value - i, false)
          cycle(s"full width $value freeze", Stimulus(), value - 5, false)
          cycle(s"full width $value consume hold", Stimulus(tick = true, consume = true), value - 5, false)
          cycle(s"full width $value resume", Stimulus(tick = true), value - 6, false)
        }
      }

      group {
        cycle("reset beats active write consume and tick", Stimulus(tick = true, write = Some(maxValue), consume = true, reset = true), 0, false)
        cycle("held reset ignores write", Stimulus(write = Some(1), reset = true), 0, false)
        cycle("release reset", Stimulus(tick = true), 0, false)
        cycle("arm before pending reset", Stimulus(write = Some(1)), 1, false)
        cycle("expire before pending reset", Stimulus(tick = true), 0, true)
        cycle("reset clears pending despite write", Stimulus(tick = true, write = Some(2), consume = true, reset = true), 0, false)
      }

      group {
        cycle("load before output observation", Stimulus(write = Some(2)), 2, false)
        for (i <- 1 to 8) {
          drive(Stimulus(tick = i % 2 == 0, write = Some(BigInt(i)), consume = true))
          check(s"unclocked output observation $i", 2, pending = false)
        }
        cycle("observation did not advance state", Stimulus(tick = true), 1, false)
        cycle("observation preserves expiration deadline", Stimulus(tick = true), 0, true)
        for (i <- 1 to 8) check(s"pending observation $i", 0, pending = true)
      }

      group {
        val random = new Random(seed)
        val boundaryValues = Vector(BigInt(0), BigInt(1), BigInt(2), BigInt(3), BigInt(7),
          BigInt(257), maxValue, maxValue - 1, BigInt(1) << 63)
        for (i <- 0 until randomCycles) {
          val write = if (random.nextInt(8) == 0) {
            Some(if (random.nextInt(4) == 0) BigInt(64, random)
              else boundaryValues(random.nextInt(boundaryValues.size)))
          } else None
          advance(s"random seed=$seed sample=$i", Stimulus(
            tick = random.nextInt(4) != 0,
            write = write,
            consume = random.nextInt(8) == 0,
            reset = random.nextInt(128) == 0
          ))
        }
      }

      println(s"UserTimer RTL PASS: groups=$groups cycles=$cycles randomCycles=$randomCycles " +
        s"seed=$seed expirationEdges=$expirationEdges backend=verilator")
    }
  }
}
