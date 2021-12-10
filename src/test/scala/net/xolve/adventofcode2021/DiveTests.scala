package net.xolve.adventofcode2021

import net.xolve.adventofcode2021.Dive.Command
import net.xolve.net.xolve.basetest.UnitSpec

class DiveTests extends UnitSpec {
  "parseLine" when {
    "forward command" should  {
      "parse" in {
        Dive.parseLIne("forward 190") shouldBe Dive.Command.Forward(190)
      }
    }

    "up command" should  {
      "parse" in {
        Dive.parseLIne("up 10") shouldBe Dive.Command.Up(10)
      }
    }

    "down command" should  {
      "parse" in {
        Dive.parseLIne("down 9") shouldBe Dive.Command.Down(9)
      }
    }

    "unknown command" should {
      "throw exception" in {
        assertThrows[IllegalArgumentException] {
          Dive.parseLIne("blah 9")
        }
      }
    }
  }

  "commandsEvaluator" should {
    "return correct position" in {
      val commands = Seq(
        Command.Forward(5),
        Command.Down(5),
        Command.Forward(8),
        Command.Up(3),
        Command.Down(8),
        Command.Forward(2),
      )

      Dive.positionEvaluator(commands.iterator) shouldBe (15, 10)
    }
  }

  "aimEvaluator" should {
    "return correct aim" in {
      val commands = Seq(
        Command.Forward(5),
        Command.Down(5),
        Command.Forward(8),
        Command.Up(3),
        Command.Down(8),
        Command.Forward(2),
      )

      Dive.aimEvaluator(commands.iterator) shouldBe (15, 60, 10)
    }
  }

  "sample test from site" which {
    val lines: String = """forward 5
                          |down 5
                          |forward 8
                          |up 3
                          |down 8
                          |forward 2
                          |""".stripMargin

    "part1" should {
      "pass" in {
        Dive.part1(lines.linesIterator) shouldBe 150
      }
    }

    "part2" should {
      "pass" in {
        Dive.part2(lines.linesIterator) shouldBe 900
      }
    }
  }
}
