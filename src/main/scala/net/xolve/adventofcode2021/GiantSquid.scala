// https://adventofcode.com/2021/day/4

package net.xolve.adventofcode2021

import java.awt.print.Book
import javax.management.ValueExp

object GiantSquid {

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def part1(): Int = {
    val (randomNumbers, boards) = parseInputFile("src/resources/input_day4.txt")
    var crossedBoard: Option[BingoBoard] = None
    val crossingNum = randomNumbers.find(num => {
      crossedBoard = boards.find(_.markAndCheck(num))
      crossedBoard.isDefined
    })

    (crossedBoard, crossingNum) match {
      case (Some(board), Some(num)) =>
        board.matrix.map(_.filterNot(_.crossed).map(_.value).sum).sum * num
      case _ => throw new Exception("No rows or columns crossed :-(")
    }
  }

  def part2(): Int = {
    val (randomNumbers, boards) = parseInputFile("src/resources/input_day4.txt")
    var crossedBoardNum: Option[(BingoBoard, Int)] = None
    randomNumbers.foreach(num => {
      boards
        .filterNot(b => b.crossedRow || b.crossedCol)
        .map(b => (b, num, b.markAndCheck(num)))
        .findLast(_._3)
        .map(t => (t._1, t._2))
        .foreach(bn => crossedBoardNum = Some(bn))
    })

    crossedBoardNum match {
      case Some(board, num) =>
        board.matrix.map(_.filterNot(_.crossed).map(_.value).sum).sum * num
      case _ => throw new Exception("No rows or columns crossed :-(")
    }
  }

  def parseInputFile(filePath: String): (Seq[Int], Seq[BingoBoard]) = {
    import scala.collection.mutable
    import scala.io.Source
    import scala.util.Using

    Using(Source.fromFile(filePath)) { f =>
      val lineIterator = f.getLines()
      val randomNumbers = lineIterator.next().split(",").map(_.toInt).toSeq

      println(lineIterator.next())

      val boards = mutable.ListBuffer[BingoBoard]()

      while lineIterator.hasNext do {
        val lines = lineIterator.takeWhile(_.nonEmpty).toSeq
        require(lines.length == 5, s"${lines}")
        boards addOne new BingoBoard(lines)
      }

      (randomNumbers, boards.toSeq)
    }.get
  }

  class BingoBoard(lines: Seq[String]) {
    val matrix: Array[Array[Cell]] = {
      val matrix = lines.map { line =>
        line
          .split("\\s+")
          .filterNot(_.isEmpty)
          .map(n => Cell(n.toInt))
      }.toArray

      require(matrix.length == 5)
      matrix.foreach(row => require(row.length == 5))
      matrix
    }

    def markAndCheck(value: Int): Boolean = {
      markCell(value)
      crossedRow || crossedCol
    }

    def crossedRow: Boolean = {
      matrix.exists(row => row.forall(_.crossed))
    }

    def crossedCol: Boolean = {
      val rowSize = matrix.length
      val colSize = matrix.head.length
      (0 until colSize)
        .map { colIdx =>
          (0 until rowSize).map(rowIdx => matrix(rowIdx)(colIdx))
        }
        .exists(col => col.forall(_.crossed))
    }

    def markCell(value: Int): Unit = {
      matrix foreach { row =>
        row foreach { cell =>
          if cell.value == value then { cell.crossed = true }
        }
      }
    }

    override def toString: String = {
      matrix
        .map(row => row.map(cell => cell.toString).mkString(" "))
        .mkString("\n")
    }

    case class Cell(value: Int) {
      var crossed: Boolean = false

      override def toString: String = {
        if crossed then { String.format("%2s", "X") }
        else { String.format("%2d", value) }
      }
    }
  }
}
