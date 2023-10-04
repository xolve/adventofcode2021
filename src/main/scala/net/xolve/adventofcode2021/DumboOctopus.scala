package net.xolve.adventofcode2021

object DumboOctopus extends App:

  import scala.collection.mutable

  private trait OctopusState

  private case class NormalGlow(level: Int) extends OctopusState:
    def increment(): NormalGlow =
      NormalGlow(level + 1)
    end increment

    override def toString: String = level.toString
  end NormalGlow

  private case object Glowed extends OctopusState

  private object OctopusState:
    def apply(level: Int): OctopusState =
      NormalGlow(level)
    end apply
  end OctopusState

  private def readInput(): Array[Array[OctopusState]] =
    import scala.io.Source
    import scala.util.Using
    Using(Source.fromFile("src/resources/input_day11.txt")): fp =>
        fp.getLines().map: line =>
          line.map(c => OctopusState(c - '0')).toArray
        .toArray
      .get
  end readInput

  private def neighbours(r: Int, c: Int, rowsNum: Int, columnsNUm: Int): Iterator[(Int, Int)] =
    val deltas = List(
      (-1, -1), (-1, 0), (-1, 1),
      (0, -1), (0, 1),
      (1, -1), (1, 0), (1, 1),
    )
    for case (dr, dc) <- deltas.iterator
      nr = r + dr
      nc = c + dc
      if 0 <= nr && nr < rowsNum && 0 <= nc && nc < columnsNUm
    yield (nr, nc)

  private def step(input: Array[Array[OctopusState]]): Int =
    val waitingToGlow = new mutable.HashSet[(Int, Int)]

    for i <- input.indices do
      for j <- input.head.indices do
        input(i)(j) = input(i)(j) match
          case normalGlow: NormalGlow =>
            val newGlow = normalGlow.increment()
            if newGlow.level > 9 then waitingToGlow.add((i, j))
            newGlow
          case Glowed => Glowed

    while waitingToGlow.nonEmpty do
      val (currR, currC) = waitingToGlow.take(1).head
      waitingToGlow.remove((currR, currC))
      input(currR)(currC) = Glowed

      neighbours(currR, currC, input.length, input.head.length).foreach: (i, j) =>
        input(i)(j) = input(i)(j) match
          case normalGlow: NormalGlow =>
            val newGlow = normalGlow.increment()
            if newGlow.level > 9 then
              waitingToGlow.add((i, j))
            newGlow
          case Glowed => Glowed

    val glowCount = input.map(_.count(_ == Glowed)).sum

    for i <- input.indices do
      for j <- input.head.indices do
        if input(i)(j) == Glowed then input(i)(j) = NormalGlow(0)

    glowCount
  end step

  private def part1(input: Array[Array[OctopusState]]): Int =
    (0 until 100).map(_ => step(input)).sum
  end part1

  private def part2(input: Array[Array[OctopusState]]): Int =
    (1 to Int.MaxValue).find(i => step(input) == input.length * input.head.length).get
  end part2

  {
    val inputInts = Array(
      Array(1,1,1,1,1),
      Array(1,9,9,9,1),
      Array(1,9,1,9,1),
      Array(1,9,9,9,1),
      Array(1,1,1,1,1),
    )

    val input: Array[Array[OctopusState]] = inputInts.map(row => row.map(NormalGlow(_)))

    step(input)
    input.foreach(row => println(row.toList))
    println("---")

    step(input)
    input.foreach(row => println(row.toList))
    println("---")
  }

  {
    val input = readInput()
    println(part1(input))
  }

  {
    val input = readInput()
    println(part2(input))
  }

end DumboOctopus
