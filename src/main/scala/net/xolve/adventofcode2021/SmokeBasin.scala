package net.xolve.adventofcode2021

object SmokeBasin extends App {

  private def readPuzzleInput(): Array[Array[Int]] =
    import scala.util.Using
    import scala.io.Source

    Using(Source.fromFile("src/resources/input_day9.txt")) { f =>
      f.getLines().map { line =>
        line.chars.map(_ - '0').toArray
      }.toArray
    }.get

  private def getNeighboursCoordinates(x: Int, y: Int, rowsNum: Int, colsNum: Int): Iterator[(Int, Int)] =
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))
    for case (dx, dy) <- directions.iterator
      nx = x + dx
      ny = y + dy
      if nx >= 0 && nx < rowsNum && ny >= 0 && ny < colsNum
    yield (nx, ny)

  private def getNeighbours[T](x: Int, y: Int, input: Array[Array[T]]): Iterator[T] =
    for case (i, j) <- getNeighboursCoordinates(x, y, input.length, input.head.length)
    yield input(i)(j)

  private def findAllLowPoints(input: Array[Array[Int]]): IndexedSeq[Int] =
    for x <- input.indices
        y <- input.head.indices
        if getNeighbours(x, y, input).forall(input(x)(y) < _)
    yield input(x)(y)

  def part1(): Int =
    findAllLowPoints(readPuzzleInput()).map(_ + 1).sum

  {
    val input = Array(
      Array(2, 1, 9, 9, 9, 4, 3, 2, 1, 0),
      Array(3, 9, 8, 7, 8, 9, 4, 9, 2, 1),
      Array(9, 8, 5, 6, 7, 8, 9, 8, 9, 2),
      Array(8, 7, 6, 7, 8, 9, 6, 7, 8, 9),
      Array(9, 8, 9, 9, 9, 6, 5, 6, 7, 8),
    )

    println(findAllLowPoints(input).toList)
  }

  {
    println(part1())
  }
}