// https://adventofcode.com/2021/day/1

package net.xolve.adventofcode2021

object SonarSweep {

  import scala.io.Source
  import scala.util.Using

  def countIncreases(depths: Iterator[Int]): Int = {
    depths.sliding(2).count(x => x(1) > x(0))
  }

  def part1(): Int = {
    Using(Source.fromFile("src/resources/input_day1.txt")) { f =>
      countIncreases(f.getLines().map(_.toInt))
    }.get
  }

  def sumKIncreases(depths: Iterator[Int], k: Int): Int = {
    countIncreases(depths.sliding(k).map(_.sum))
  }

  def part2(): Int = {
    Using(Source.fromFile("src/resources/input_day1.txt")) { f =>
      sumKIncreases(f.getLines().map(_.toInt), 3)
    }.get
  }
}

@main def main() = {
  println(SonarSweep.part1())
  println(SonarSweep.part2())
}
