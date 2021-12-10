package net.xolve.adventofcode2021

import net.xolve.net.xolve.basetest.UnitSpec

class BinaryDiagnosticTests extends UnitSpec {
  val lines: Seq[String] = """00100
                |11110
                |10110
                |10111
                |10101
                |01111
                |00111
                |11100
                |10000
                |11001
                |00010
                |01010""".stripMargin.linesIterator.toSeq

  "counts" should {
    "give proper counts" when {
      "empty input" in {
        val lines = Seq()
        BinaryDiagnostic.countsAtAllPos(lines.iterator) shouldBe Map()
      }

      "single line" in {
        val lines = Seq("abcdaabc")
        val expected = Map(
          0 -> Map('a' -> 1),
          1 -> Map('b' -> 1),
          2 -> Map('c' -> 1),
          3 -> Map('d' -> 1),
          4 -> Map('a' -> 1),
          5 -> Map('a' -> 1),
          6 -> Map('b' -> 1),
          7 -> Map('c' -> 1),
        )
        BinaryDiagnostic.countsAtAllPos(lines.iterator) shouldBe expected
      }

      "multiple lines" in {
        val expected = Map(
          0 -> Map('0' -> 5, '1' -> 7),
          1 -> Map('0' -> 7, '1' -> 5),
          2 -> Map('0' -> 4, '1' -> 8),
          3 -> Map('0' -> 5, '1' -> 7),
          4 -> Map('0' -> 7, '1' -> 5),
        )
        BinaryDiagnostic.countsAtAllPos(lines.iterator) shouldBe expected
      }
    }
  }

  "majority" should {
    "return majority element" in {
      val map = Map('0' -> 10, '1' -> 20)
      BinaryDiagnostic.majority(map) shouldBe '1'
    }

    "return 1" when {
      "tie" in {
        val map = Map('0' -> 10, '1' -> 10)
        BinaryDiagnostic.majority(map) shouldBe '1'
      }
    }
  }

  "minority" should {
    "return minority element" in {
      val map = Map('0' -> 10, '1' -> 20)
      BinaryDiagnostic.minority(map) shouldBe '0'
    }

    "return 0" when {
      "tie" in {
        val map = Map('0' -> 10, '1' -> 10)
        BinaryDiagnostic.minority(map) shouldBe '0'
      }
    }
  }

  "part1" should {
    "return correct answer" in {
      BinaryDiagnostic.part1(lines.iterator) shouldBe 198
    }
  }

  "countAndKeep" should {
    "return correct answer" when {
      "majority selection" in {
        BinaryDiagnostic.countAndKeep(lines, 0)(BinaryDiagnostic.majority) shouldBe "10111"
      }

      "minority selection" in {
        BinaryDiagnostic.countAndKeep(lines, 0)(BinaryDiagnostic.minority) shouldBe "01010"
      }
    }
  }
}
