// https://adventofcode.com/2021/day/5

package net.xolve.adventofcode2021

object HydrothermalVenture {
  def main(args: Array[String]): Unit = {
    val lines = parseInputFile("src/resources/input_day5.txt")
    println(part1(lines))
    println(part2(lines))
  }

  def parseInputFile(file: String): Seq[Line] = {
    import scala.io.Source
    import scala.util.Using

    Using(Source.fromFile(file)) { f =>
      f.getLines()
        .map {
          case s"$x0,$y0 -> $x1,$y1" =>
            Line.create(Point(x0.toInt, y0.toInt), Point(x1.toInt, y1.toInt))
          case line =>
            throw new IllegalArgumentException(s"$line is in invalid format.")
        }
        .toSeq
    }.get
  }

  def part1(lines: Seq[Line]): Int = {
    lines
      .flatMap(line =>
        if line.p0.x == line.p1.x then
          (line.p0.y to line.p1.y).map(Point(line.p0.x, _))
        else if line.p0.y == line.p1.y then
          (line.p0.x to line.p1.x).map(Point(_, line.p0.y))
        else Seq()
      )
      .groupBy(p => p)
      .count(_._2.length > 1)
  }

  def part2(lines: Seq[Line]): Int = {
    lines
      .flatMap(line =>
        if line.p0.x == line.p1.x then
          (line.p0.y to line.p1.y).map(Point(line.p0.x, _))
        else if line.p0.y == line.p1.y then
          (line.p0.x to line.p1.x).map(Point(_, line.p0.y))
        else if line.p0.y < line.p1.y then
          (line.p0.x to line.p1.x)
            .zip((line.p0.y to line.p1.y))
            .map((x, y) => Point(x, y))
        else if line.p0.y > line.p1.y then
          (line.p0.x to line.p1.x)
            .zip((line.p0.y to line.p1.y by -1))
            .map((x, y) => Point(x, y))
        else throw Exception(s"Cannot figure out line $line")
      )
      .groupBy(p => p)
      .count(_._2.length > 1)
  }

  case class Point(x: Int, y: Int)

  case class Line(p0: Point, p1: Point)

  object Line {
    def create(p0: Point, p1: Point): Line = {
      if p1.x < p0.x || (p1.x == p0.x && p1.y < p0.y) then {
        Line(p1, p0)
      } else {
        Line(p0, p1)
      }
    }
  }
}
