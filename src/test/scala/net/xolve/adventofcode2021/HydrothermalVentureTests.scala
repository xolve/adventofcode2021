package net.xolve.adventofcode2021

import net.xolve.net.xolve.basetest.UnitSpec

class HydrothermalVentureTests extends UnitSpec {

  import HydrothermalVenture.*

  "part1" should {
    "return correct answer" when {
      "no line" in {
        val lines = Seq()
        HydrothermalVenture.part1(lines) shouldBe 0
      }

      "single line" in {
        val lines = Seq(Line(Point(5, 0), Point(5, 10)))
        HydrothermalVenture.part1(lines) shouldBe 0
      }

      "one crossing line" in {
        val lines =
          Seq(Line(Point(5, 0), Point(5, 10)), Line(Point(3, 2), Point(12, 2)))
        HydrothermalVenture.part1(lines) shouldBe 1
      }

      "Diagonal lines" in {
        val lines = Seq(
          Line(Point(5, 0), Point(5, 10)),
          Line(Point(3, 2), Point(12, 2)),
          Line(Point(2, 2), Point(10, 10))
        )
        HydrothermalVenture.part1(lines) shouldBe 1
      }
    }
  }

}
