package net.xolve.adventofcode2021

object SyntaxScoring extends App:

  import scala.collection.mutable
  import scala.util.boundary
  import scala.util.boundary.break

  private def readInput(): List[String] =
    import scala.io.Source
    import scala.util.Using

    Using(Source.fromFile("src/resources/input_day10.txt")): fp =>
      fp.getLines().toList
    .get

  private val unmatchedScoreMap = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )

  private def closingScoreMap = Map(
    '(' -> 1,
    '[' -> 2,
    '{' -> 3,
    '<' -> 4,
  )

  private def firstUnmatchingCharOrUnclosedString(line: String): Either[Char, List[Char]] =
    val stack = new mutable.Stack[Char]
    boundary:
      for c <- line do
        c match
          case '(' | '[' | '{' | '<' => stack.push(c)
          case ')' => if stack.pop() != '(' then break(Left(')'))
          case ']' => if stack.pop() != '[' then break(Left(']'))
          case '}' => if stack.pop() != '{' then break(Left('}'))
          case '>' => if stack.pop() != '<' then break(Left('>'))
      Right(stack.toList)

  private def part1(lines: List[String]): Int =
    lines.flatMap(firstUnmatchingCharOrUnclosedString(_).left.toOption).map(unmatchedScoreMap(_)).sum

  private def part2(lines: List[String]): Long =
    val sortedScores = lines.map(firstUnmatchingCharOrUnclosedString).filter(_.isRight).map { er =>
        val leftOverString = er.getOrElse(List.empty)
        leftOverString.foldLeft(0L)((acc, c) => acc * 5 + closingScoreMap(c))
      }.sorted

    sortedScores(sortedScores.length / 2)

  {
    val lines = List(
        "[({(<(())[]>[[{[]{<()<>>",
        "[(()[<>])]({[<{<<[]>>(",
        "{([(<{}[<>[]}>{[]{[(<()>",
        "(((({<>}<{<{<>}{[]{[]{}",
        "[[<[([]))<([[{}[[()]]]",
        "[{[{({}]{}}([{[{{{}}([]",
        "{<[[]]>}<{[{[{[]{()[[[]",
        "[<(<(<(<{}))><([]([]()",
        "<{([([[(<>()){}]>(<<{{",
        "<{([{{}}[<[[[<>{}]]]>[]]",
    )
    println(part1(lines))
    println(part2(lines))
  }

  {
    val input = readInput()
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }
end SyntaxScoring
