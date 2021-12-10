package net.xolve.adventofcode2021

import net.xolve.adventofcode2021.SonarSweep
import net.xolve.net.xolve.basetest.UnitSpec

class SonarSweepTests extends UnitSpec {
  "countIncreases" should {
    "count correct increases" in {
      val depths = Seq(0, 0, 1, 2, 2, 2, 1, 1, 3)
      SonarSweep.countIncreases(depths.iterator) shouldBe 3
    }
  }

  "sumKIncreases" should {
    "count correct sum increases" in {
      val depths = Seq(0, 0, 1, 2, 2, 2, 1, 1, 3)
      SonarSweep.sumKIncreases(depths.iterator, 4) shouldBe 3
    }
  }
}
