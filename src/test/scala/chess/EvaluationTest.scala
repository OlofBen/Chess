package chess

import org.scalatest.funsuite.AnyFunSuite
import chess.pieces._

class EvaluationFunSuite extends AnyFunSuite:
  test("A new game should have a score of 0") {
    val board = Board.startingPosition()
    require(Evaluation.evaluate(board) == 0)
  }
  
  test("Two white rooks and two kings") {
    val pieces = Seq(
      King(Position("a1"), Color.White),
      King(Position("a8"), Color.Black),
      Rook(Position("h6"), Color.White),
      Rook(Position("h7"), Color.White)
    )
    val board = Board.emptyWith(pieces)
    val eval = Evaluation.evaluate(board)
    require(eval == 10, s"Two rooks more are 10 points, current eval $eval")
  }

  test("Black is checkmated") {
    val pieces = Seq(
      King(Position("a1"), Color.White),
      King(Position("a8"), Color.Black),
      Rook(Position("h8"), Color.White),
      Rook(Position("h7"), Color.White)
    )
    val board = Board.emptyWith(pieces).nextTurn()
    val eval = Evaluation.evaluate(board)
    require(eval == Int.MaxValue)
  }