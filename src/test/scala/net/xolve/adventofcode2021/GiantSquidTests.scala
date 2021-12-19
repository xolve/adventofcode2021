package net.xolve.adventofcode2021

import net.xolve.adventofcode2021.GiantSquid.BingoBoard
import net.xolve.net.xolve.basetest.UnitSpec

class GiantSquidTests extends UnitSpec {

  val lines: Seq[String] =
    """22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19""".stripMargin.linesIterator.toSeq

  val expected: Array[Array[Int]] = Array(
    Array(22, 13, 17, 11, 0),
    Array(8, 2, 23, 4, 24),
    Array(21, 9, 14, 16, 7),
    Array(6, 10, 3, 18, 5),
    Array(1, 12, 20, 15, 19)
  )

  "BingoBoard" should {
    "be well formed" when {
      "created" in {
        val board = new BingoBoard(lines)
        board.matrix.zip(expected).foreach { ca =>
          val cs = ca._1
          val is = ca._2
          cs.map(_.value) shouldBe is
        }
      }
    }

    "marked" when {
      "number exists" in {
        val board = new BingoBoard(lines)
        board.markCell(16)
        board.matrix(2)(3).crossed shouldBe true
      }
    }

    "not marked" when {
      "number not exists" in {
        val board = new BingoBoard(lines)
        board.markCell(99)
        board.matrix foreach { row =>
          row.forall(_.crossed == false)
        }
      }
    }

    "return true for crossed row" when {
      "row is crossed" in {
        val board = new BingoBoard(lines)
        expected(2).foreach(board.markCell)
        board.crossedRow shouldBe true
      }
    }

    "return true for cross column" when {
      "column is crossed" in {
        val board = new BingoBoard(lines)
        val col = Array(22, 8, 21, 6, 1)
        col.foreach(board.markCell)
        board.crossedCol shouldBe true
      }
    }

    "markAndCheck" should {
      "not return crossed" when {
        "nothing is crossed" in {
          val board = new BingoBoard(lines)
          board.markAndCheck(22) shouldBe false
          board.markAndCheck(9) shouldBe false
          board.markAndCheck(16) shouldBe false
          board.markAndCheck(10) shouldBe false
          board.markAndCheck(15) shouldBe false
        }
      }

      "return true for crossed row" when {
        "row is crossed" in {
          val board = new BingoBoard(lines)
          expected(2).map(board.markAndCheck).last shouldBe true
        }
      }

      "return true for crossed column" when {
        "column is crossed" in {
          val board = new BingoBoard(lines)
          val expected = Array(22, 8, 21, 6, 1)
          expected.map(board.markAndCheck).last shouldBe true
        }
      }
    }
  }

}
