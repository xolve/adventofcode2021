package net.xolve.adventofcode2021

import net.xolve.net.xolve.basetest.UnitSpec

class TheTreacheryOfWhalesTests extends UnitSpec {

  import TheTreacheryOfWhales.*

  val singleElement = Seq(10)
  val twoElement = Seq(10, 5)
  val example = Seq(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)

  "part1" should {
    "return correct result" when {
      "empty list" in {
        part1(Seq()) shouldBe 0
      }

      "single element" in {
        part1(singleElement) shouldBe 0
      }

      "two element" in {
        part1(twoElement) shouldBe 5
      }

      "example from site" in {
        part1(example) shouldBe 37
      }
    }
  }

  "part2" should {
    "return correct result" when {
      "empty list" in {
        part2(Seq()) shouldBe 0
      }

      "single element" in {
        part2(singleElement) shouldBe 0
      }

      "two element" in {
        part2(twoElement) shouldBe 9
      }

      "example from site" in {
        part2(example) shouldBe 168
      }
    }
  }

}
