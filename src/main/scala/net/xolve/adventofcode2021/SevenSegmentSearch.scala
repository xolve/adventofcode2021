// https://adventofcode.com/2021/day/8

package net.xolve.adventofcode2021

object SevenSegmentSearch {

  case class InputLine(inputs: Seq[String], outputs: Seq[String])

  def parseInputFile(fileName: String): Seq[InputLine] = {
    import scala.io.Source
    import scala.util.Using

    Using(Source.fromFile(fileName)) { f =>
      f.getLines()
        .map { line =>
          val iter = line.split("\\s+").iterator
          val inputs = iter.takeWhile(_ != "|").toSeq
          val outputs = iter.toSeq
          InputLine(inputs, outputs)
        }
        .toSeq
    }.get
  }

  val fixedSegmentCountNums = Map(
    2 -> 1,
    4 -> 4,
    3 -> 7,
    7 -> 8,
  )

  def part1(lines: Seq[InputLine]): Int = {
    val counts = lines
      .flatMap(_.outputs)
      .map(_.length)
      .filter(fixedSegmentCountNums.contains)
      .groupMapReduce(i => i)(i => 1)(_ + _)

    counts.values.sum
  }

  def main(args: Array[String]): Unit = {
    val inputLines = parseInputFile("src/resources/input_day8.txt")
    println(part1(inputLines))
  }

}
