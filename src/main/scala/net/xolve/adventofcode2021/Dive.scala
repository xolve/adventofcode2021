// https://adventofcode.com/2021/day/2

package net.xolve.adventofcode2021

object Dive {
  import scala.util.Using;
  import scala.io.Source;

  enum Command:
    case Forward(value: Int) extends Command
    case Up(value: Int) extends Command
    case Down(value: Int) extends Command

  def parseLIne(line: String): Command = {
    line match {
      case s"forward $value" => Command.Forward(value.toInt)
      case s"up $value"      => Command.Up(value.toInt)
      case s"down $value"    => Command.Down(value.toInt)
      case _ => throw new IllegalArgumentException(s"Unknown command $line.")
    }
  }

  def positionEvaluator(commands: Iterator[Command]): (Int, Int) = {
    commands
      .foldLeft((0, 0))((pos, cmd) => {
        val (h, d) = pos
        cmd match {
          case Command.Forward(value) => (h + value, d)
          case Command.Up(value)      => (h, d - value)
          case Command.Down(value)    => (h, d + value)
        }
      })
  }

  def part1(lines: Iterator[String]): Int = {
    val (h, d) = positionEvaluator(lines.map(parseLIne))
    h * d
  }

  def aimEvaluator(commands: Iterator[Command]): (Int, Int, Int) = {
    commands
      .foldLeft((0, 0, 0))((pos, cmd) => {
        val (h, d, a) = pos
        cmd match {
          case Command.Forward(value) => (h + value, d + (a * value), a)
          case Command.Up(value)      => (h, d, a - value)
          case Command.Down(value)    => (h, d, a + value)
        }
      })
  }

  def part2(lines: Iterator[String]): Int = {
    val (h, d, a) = aimEvaluator(lines.map(parseLIne))
    h * d
  }

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("src/resources/input_day2.txt")) { f =>
      println(part1(f.getLines()))
    }

    Using(Source.fromFile("src/resources/input_day2.txt")) { f =>
      println(part2(f.getLines()))
    }
  }
}
