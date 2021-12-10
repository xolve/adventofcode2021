// https://adventofcode.com/2021/day/3

package net.xolve.adventofcode2021

import scala.annotation.tailrec

object BinaryDiagnostic {
  import scala.collection.SortedMap

  def countsAtAllPos(lines: Iterator[String]): SortedMap[Int, Map[Char, Int]] = {
    import scala.collection.mutable
    val indexedMaps = mutable.Map[Int, mutable.Map[Char, Int]]()
    lines
      .foreach { line =>
        line.zipWithIndex.foreach { (c, i) =>
          val mapAtIndex = indexedMaps.getOrElseUpdate(i, mutable.Map() withDefaultValue 0)
          mapAtIndex.addOne(c -> (mapAtIndex(c) + 1))
        }
      }
    SortedMap[Int, Map[Char, Int]]() ++ indexedMaps.map(e => (e._1, e._2.toMap))
  }

  def majority(counts: Map[Char, Int]): Char = {
    val cz = counts withDefaultValue 0
    if cz('0') == cz('1') then '1' else cz.maxBy(_._2)._1
  }

  def minority(counts: Map[Char, Int]): Char = {
    val cz = counts withDefaultValue 0
    if cz('0') == cz('1') then '0' else cz.minBy(_._2)._1
  }

  def part1(lines: Iterator[String]): Int = {
    val counts = BinaryDiagnostic.countsAtAllPos(lines)
    val gammaRate = Integer.parseInt(counts.map((_, m) => majority(m)).mkString, 2)
    val epsilonRate = Integer.parseInt(counts.map((_, m) => minority(m)).mkString, 2)
    gammaRate * epsilonRate
  }

  @tailrec
  def countAndKeep(lines: Seq[String], index: Int)(votingFunc: Map[Char, Int] => Char): String = {
    if lines.length == 1 then {
      lines.head
    } else {
      import scala.collection.mutable

      val counts = mutable.Map[Char, Int]() withDefaultValue 0
      lines.map(_.charAt(index)).foreach(c => {
        counts.addOne(c -> (counts(c) + 1))
      })

      val selectedMajority = votingFunc(counts.toMap)
      val nextLines = lines.filter(line => line.charAt(index) == selectedMajority)
      countAndKeep(nextLines, index + 1)(votingFunc)
    }
  }

  def part2(lines: Seq[String]): Int = {
    val o2Metric = Integer.parseInt(countAndKeep(lines, 0)(majority), 2)
    val co2Metric = Integer.parseInt(countAndKeep(lines, 0)(minority), 2)
    o2Metric * co2Metric
  }

  def main(args: Array[String]): Unit = {
    import scala.util.Using
    import scala.io.Source

    Using(Source.fromFile("src/resources/input_day3.txt")) { f =>
      println(part1(f.getLines()))
    }

    Using(Source.fromFile("src/resources/input_day3.txt")) { f =>
      println(part2(f.getLines().toSeq))
    }
  }
}
