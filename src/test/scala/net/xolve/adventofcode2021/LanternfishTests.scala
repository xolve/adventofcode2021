package net.xolve.adventofcode2021

import net.xolve.net.xolve.basetest.UnitSpec

class LanternfishTests extends UnitSpec {

  import Lanternfish.*

  "Fish" should {
    "produce another fish" when {
      "day 6 is reached" in {
        val fish = new Fish(3)

        fish.increment() shouldBe None
        fish.timer shouldBe 2

        fish.increment() shouldBe None
        fish.timer shouldBe 1

        fish.increment() shouldBe None
        fish.timer shouldBe 0

        val expectedNewFish = fish.increment()
        expectedNewFish.value shouldBe a[Fish]
        expectedNewFish.value.timer shouldBe 8
        fish.timer shouldBe 6
      }
    }
  }

  "part1" should {
    "return correct answer" when {
      "single fish" in {
        val fishes = Seq(new Fish(3))
        part1(fishes, 4).length shouldBe 2
      }

      "sample example" in {
        val fishes = Seq(3, 4, 3, 1, 2).map(new Fish(_))
        part1(fishes, 18).length shouldBe 26
      }
    }
  }

  "part2" should {
    "return correct answer" when {
      "single fish" in {
        val fishes = Seq(3)
        part2(fishes, 4) shouldBe 2
      }

      "multiple fish" in {
        val fishes = Seq(3, 3, 3, 3, 3)
        part2(fishes, 4) shouldBe 10
      }

      "sample example with 256 days" in {
        val fishes = Seq(3, 4, 3, 1, 2)
        part2(fishes, 256) shouldBe 26984457539L
      }
    }
  }
}
