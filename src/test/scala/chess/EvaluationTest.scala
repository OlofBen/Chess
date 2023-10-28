package chess

import org.scalatest.funsuite.AnyFunSuite

class EvaluationFunSuite extends AnyFunSuite:
  test("A new game should have a score of 0") {
    val board = Board.startingPosition()
    require(Evaluation.evaluate(board) == 0)
  }
  