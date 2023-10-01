package net.xolve.adventofcode2021

object SyntaxScoring extends App:

  import scala.util.boundary
  import scala.util.boundary.break

  private def readInput(): List[String] =
    import scala.io.Source
    import scala.util.Using

    Using(Source.fromFile("src/resources/input_day10.txt")): fp =>
      fp.getLines().toList
    .get

  private def firstUnmatchingChar(line: String): Option[Char] =
    import scala.collection.mutable
    val stack = new mutable.Stack[Char]
    boundary:
      for c <- line do
        c match
          case '(' | '[' | '{' | '<' => stack.push(c)
          case ')' => if stack.pop() != '(' then break(Some(')'))
          case ']' => if stack.pop() != '[' then break(Some(']'))
          case '}' => if stack.pop() != '{' then break(Some('}'))
          case '>' => if stack.pop() != '<' then break(Some('>'))
      None

  private def part1(lines: List[String]): Int =
    val scoreMap = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137,
    ) withDefaultValue 0
    lines.flatMap(firstUnmatchingChar).map(scoreMap).sum
  
  {
    val input = readInput()
    println(s"Part 1: ${part1(input)}")
  }
end SyntaxScoring
