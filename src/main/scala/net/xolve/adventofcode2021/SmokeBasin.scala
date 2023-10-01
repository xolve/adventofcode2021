package net.xolve.adventofcode2021

object SmokeBasin extends App {

  import scala.collection.mutable

  private case class Point(x: Int, y: Int)

  private def readPuzzleInput(): Array[Array[Int]] =
    import scala.util.Using
    import scala.io.Source

    Using(Source.fromFile("src/resources/input_day9.txt")) { f =>
      f.getLines().map { line =>
        line.chars.map(_ - '0').toArray
      }.toArray
    }.get

  private def getNeighbouringPoints(point: Point, rowsNum: Int, colsNum: Int): Iterator[Point] =
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))
    for case (dx, dy) <- directions.iterator
      nx = point.x + dx
      ny = point.y + dy
      if nx >= 0 && nx < rowsNum && ny >= 0 && ny < colsNum
    yield Point(nx, ny)

  private def getNeighbours[T](point: Point, input: Array[Array[T]]): Iterator[T] =
    for
      case Point(x, y) <- getNeighbouringPoints(point, input.length, input.head.length)
    yield input(x)(y)

  private def findAllLowPoints(input: Array[Array[Int]]): IndexedSeq[Point] =
    for
      x <- input.indices
      y <- input.head.indices
      if getNeighbours(Point(x, y), input).forall(input(x)(y) < _)
    yield
      Point(x, y)

  private def growLowPoint(puzzleInput: Array[Array[Int]], point: Point): Set[Point] =

    def growInternal(curr: Set[Point], seen: mutable.HashSet[Point]): Set[Point] =
      val next = curr.flatMap { p => getNeighbouringPoints(p, puzzleInput.length, puzzleInput.head.length) }
        .filterNot { p => seen.contains(p) }
        .filter { (p: Point) => puzzleInput(p.x)(p.y) < 9 }
      seen ++= curr
      next

    var current = Set(point)
    val seen = new mutable.HashSet[Point]
    while current.nonEmpty do
      current = growInternal(current, seen)
    seen.toSet

  private def connectedComponents(puzzleInput: Array[Array[Int]]): Set[Set[Point]] =
    val retVal = new mutable.HashSet[Set[Point]]
    val lowPoints = new mutable.HashSet[Point] ++ findAllLowPoints(puzzleInput)
    while lowPoints.nonEmpty do
      val newConnectedComponent = growLowPoint(puzzleInput, lowPoints.head)
      lowPoints.filterInPlace(point =>  !newConnectedComponent.contains(point))
      retVal += newConnectedComponent
    retVal.toSet

  def part1(puzzleInput: Array[Array[Int]]): Int =
    findAllLowPoints(puzzleInput).map { case Point(x, y) => puzzleInput(x)(y) + 1 }.sum

  def part2(puzzleInput: Array[Array[Int]]): Int =
    connectedComponents(puzzleInput).toList.map(_.size).sorted.reverse.take(3).product

  {
    val input = Array(
      Array(2, 1, 9, 9, 9, 4, 3, 2, 1, 0),
      Array(3, 9, 8, 7, 8, 9, 4, 9, 2, 1),
      Array(9, 8, 5, 6, 7, 8, 9, 8, 9, 2),
      Array(8, 7, 6, 7, 8, 9, 6, 7, 8, 9),
      Array(9, 8, 9, 9, 9, 6, 5, 6, 7, 8),
    )

    println(part1(input))
    println(part2(input))
  }

  {
    val puzzleInput = readPuzzleInput()
    println(part1(puzzleInput))
    println(part2(puzzleInput))
  }
}