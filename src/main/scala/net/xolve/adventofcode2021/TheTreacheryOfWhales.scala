// https://adventofcode.com/2021/day/7

package net.xolve.adventofcode2021

object TheTreacheryOfWhales {

  def main(args: Array[String]): Unit = {
    import scala.io.Source
    import scala.util.Using

      val numbers = Using(Source.fromFile("src/resources/input_day7.txt")) { f =>
      f.getLines().next().split(",").map(_.toInt).toSeq
    }.get

    println(part1(numbers))
    println(part2(numbers))
  }

  def part1(numbers: Seq[Int]): Int = makeAllSameAsMedian(numbers)

  def makeAllSameAsMedian(numbers: Seq[Int]): Int = {
    if numbers.nonEmpty then {
      val numArr = Array.from(numbers)
      val median = numArr.sortInPlace().drop(numArr.length / 2).head
      numArr.map(num => (median - num).abs).sum
    } else {
      0
    }
  }

  def part2(numbers: Seq[Int]): Int = makeAllSameAsAverage(numbers)

  // Was able to use differentiation to get to mean as solution.
  // I referred to following links to fix my solution about using the margin around mean:
  // https://www.reddit.com/r/adventofcode/comments/rawxad/2021_day_7_part_2_i_wrote_a_paper_on_todays/
  // https://www.reddit.com/r/adventofcode/comments/rav728/2021_day7_can_part2_be_done_in_a_smart_way/
  def makeAllSameAsAverage(numbers: Seq[Int]): Int = {
    def cost(num0: Int, num1: Int): Int = {
      val d = (num0 - num1).abs
      d * (d + 1) / 2
    }

    if numbers.nonEmpty then {
      val avg = numbers.sum / numbers.length
      val lower = avg - 1
      val upper = avg + 1

      (lower to upper)
        .map(res => numbers.map(num => cost(res, num)).sum)
        .min
    } else {
      0
    }
  }

}
