package engine.static_eval

import org.scalatest.funsuite.AnyFunSuite
import chess.pieces._
import chess._
import engine.static_eval.simplified_evaluation_function.Simplified_Evaluation_Function

class EvaluationFunSuite extends AnyFunSuite:
  test("A new game should have a score of 0") {
    val board = Board.startingPosition()
    require(Simplified_Evaluation_Function.evaluate(board) == 0)
  }
  

  test("Black is checkmated") {
    val pieces = Seq(
      King(Position("a1"), Color.White),
      King(Position("a8"), Color.Black),
      Rook(Position("h8"), Color.White),
      Rook(Position("h7"), Color.White)
    )
    val board = Board.emptyWith(pieces).nextTurn()
    val eval = Simplified_Evaluation_Function.evaluate(board)
    require(eval == Int.MaxValue)
  }