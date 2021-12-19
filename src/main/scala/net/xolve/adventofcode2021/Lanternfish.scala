// https://adventofcode.com/2021/day/6

package net.xolve.adventofcode2021

import scala.collection.mutable.ListBuffer

object Lanternfish {

  def main(args: Array[String]): Unit = {
    {
      val fishes = parseInputFile("src/resources/input_day6.txt")
      println(part1(fishes, 80).length)
    }

    {
      val fishes = parseInputFile("src/resources/input_day6.txt").map(_.timer)
      println(part2(fishes, 256))
    }
  }

  def parseInputFile(fileName: String): Seq[Fish] = {
    import scala.io.Source
    import scala.util.Using

    Using(Source.fromFile(fileName)) { f =>
      f.getLines()
        .map { line =>
          line.split(",").map(s => new Fish(s.toInt))
        }
        .toSeq
    }.get.flatten
  }

  def part1(fishes: Seq[Fish], increments: Int): Seq[Fish] = {
    import scala.collection.mutable

    val fishesMut =
      (0 until increments).foldLeft(new ListBuffer[Fish] ++ fishes) { (fl, _) =>
        val newFishes = fl.flatMap(_.increment())
        fl :++ newFishes
      }
    fishesMut.toSeq
  }

  def part2(fishes: Seq[Int], increments: Int): Long = {
    import scala.collection.mutable

    val initialCounts: Map[Int, Long] =
      fishes.groupBy(i => i).map((k, v) => k -> v.length.toLong).toMap

    val finalCounts = (0 until increments).foldLeft(initialCounts) {
      (fishesByTimerCounts, _) =>
        val newCounts = new mutable.HashMap[Int, Long] withDefaultValue 0
        fishesByTimerCounts.foreach((timer, count) =>
          if (timer == 0) then {
            newCounts += (6 -> (newCounts(6) + count))
            newCounts += (8 -> (newCounts(8) + count))
          } else {
            newCounts += ((timer - 1) -> (newCounts(timer - 1) + count))
          }
        )
        newCounts.toMap
    }

    finalCounts.values.sum
  }

  class Fish(initTimer: Int) {
    require(initTimer >= 0 && initTimer <= 8)

    private var _timer = initTimer

    def increment(): Option[Fish] = {
      val t = timer - 1
      if t == -1 then {
        this.timer = 6
        Some(new Fish(8))
      } else {
        timer = t
        None
      }
    }

    override def toString: String = s"Fish($timer)"

    def timer: Int = _timer

    private def timer_=(timer: Int): Unit = {
      this._timer = timer
    }
  }

}
