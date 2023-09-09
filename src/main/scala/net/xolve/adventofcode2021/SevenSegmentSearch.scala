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

  def getSegmentMap(line: InputLine): Map[Char, Char] = {
    import scala.collection.mutable
    val segmentMap = new mutable.HashMap[Char, Char]

    // Identify segments for 1
    val wordFor1_cf = line.inputs.find(_.length == 2)
      .getOrElse(throw new Exception("Cannot find word for 1."))

    // Identify the top segment from 1 and 7
    val word7_acf = line.inputs.find(_.length == 3).getOrElse(throw new Exception("Cannot find word for 7."))
    val topSegment7_a = word7_acf.filterNot(x => wordFor1_cf.contains(x)).head
    segmentMap += (topSegment7_a -> 'a')

    // Find 3 and 4
    val wordFor4_bcdf = line.inputs.find(_.length == 4)
      .getOrElse(throw new Exception("Cannot find word for 4."));
    val wordFor3_acdfg = line.inputs.filter(_.length == 5).find(word => word7_acf.forall(x => word.contains(x)))
      .getOrElse(throw new Exception("Cannot find word for 3."))

    // Identify the middle horizontal segment for 3
    val middleSegment_d = wordFor3_acdfg.filterNot(word7_acf.contains).find(wordFor4_bcdf.contains)
      .getOrElse(throw new Exception("Cannot find middle segment for 3"))
    segmentMap += (middleSegment_d -> 'd')

    // Identify the top left segment for 4
    val topLeft4_b = wordFor4_bcdf.filterNot(wordFor1_cf.contains).filterNot(_ == middleSegment_d)
    require(topLeft4_b.length == 1, s"Cannot find topLeft4_b from $topLeft4_b.")
    segmentMap += (topLeft4_b.head -> 'b')

    // Identify the bottom horizontal segment for 3
    val bottom3_g = wordFor3_acdfg.filterNot(x => x == topSegment7_a || x == middleSegment_d || wordFor1_cf.contains(x))
    require(bottom3_g.length == 1, s"Cannot find bottom3_g from $bottom3_g.")
    segmentMap += (bottom3_g.head -> 'g')

    // Get segment for e
    val segment_e = "abcdefg".filterNot(x => wordFor1_cf.contains(x) || segmentMap.keys.toSet.contains(x))
    require(segment_e.length == 1, s"Cannot find segment for e from $segment_e.")
    segmentMap += (segment_e.head -> 'e')

    // Get segment for c from 2 and 1
    val wordFor2_acdefg = line.inputs.filter(_.length == 5)
      .find(x => x.contains(topSegment7_a) && x.contains(middleSegment_d) && x.contains(segment_e) && x.contains(bottom3_g))
      .getOrElse("Cannot find word for 2")
    val segment_c = wordFor1_cf.filter(wordFor2_acdefg.contains)
    require(segment_c.length == 1, s"Cannot find segment c from $segment_c.")
    segmentMap += (segment_c.head -> 'c')

    // get segment for f from 1
    val segment_f = wordFor1_cf.filterNot(_ == segment_c.head)
    require(segment_c.length == 1, s"Cannot find segment f from $segment_f.")
    segmentMap += (segment_f.head -> 'f')

    segmentMap.toMap
  }

  def part2(lines: Seq[InputLine]): Long = {
    val numberMap = Map (
      "abcefg" -> '0',
      "cf" -> '1',
      "acdeg" -> '2',
      "acdfg" -> '3',
      "bcdf" -> '4',
      "abdfg" -> '5',
      "abdefg" -> '6',
      "acf" -> '7',
      "abcdefg" -> '8',
      "abcdfg" -> '9',
    )

    lines.map(line =>
      val segmentMap = getSegmentMap(line)

      line.outputs
        .map(word => word.map(segmentMap).mkString) // map to correct segments
        .map(word => numberMap(word.sorted)) // get digits
        .mkString // make number
        .toLong // convert to long
    ).sum
  }

  def main(args: Array[String]): Unit = {
    val inputLines = parseInputFile("src/resources/input_day8.txt")
    println(part1(inputLines))

    // test case
    val inputLine = InputLine(
      Seq("acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"),
      Seq("cdfeb", "fcadb", "cdfeb", "cdbaf"))
    println(part2(Seq(inputLine)))

    println(part2(inputLines))
  }
}
