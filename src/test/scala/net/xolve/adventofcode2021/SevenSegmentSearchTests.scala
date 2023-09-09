package net.xolve.adventofcode2021

import net.xolve.net.xolve.basetest.UnitSpec

class SevenSegmentSearchTests extends UnitSpec {

  import SevenSegmentSearch._

  "part1" should {
    "return correct result" when {
      "empty input" in {
        part1(Seq()) shouldBe 0
      }

      "single output" in {
        val inputLines = Seq(InputLine(Seq(), Seq("daf")))
        part1(inputLines) shouldBe 1
      }

      "sample example" in {
        val inputLine = Seq(
          InputLine(Seq(), Seq("fdgacbe", "cefdb", "cefbgd", "gcbe")),
          InputLine(Seq(), Seq("fcgedb", "cgb", "dgebacf", "gc")),
          InputLine(Seq(), Seq("cg", "cg", "fdcagb", "cbg")),
          InputLine(Seq(), Seq("efabcd", "cedba", "gadfec", "cb")),
          InputLine(Seq(), Seq("gecf", "egdcabf", "bgf", "bfgea")),
          InputLine(Seq(), Seq("gebdcfa", "ecba", "ca", "fadegcb")),
          InputLine(Seq(), Seq("cefg", "dcbef", "fcge", "gbcadfe")),
          InputLine(Seq(), Seq("ed", "bcgafe", "cdgba", "cbgef")),
          InputLine(Seq(), Seq("gbdfcae", "bgc", "cg", "cgb")),
          InputLine(Seq(), Seq("fgae", "cfgab", "fg", "bagce"))
        )

        part1(inputLine) shouldBe 26
      }
    }
  }
}
