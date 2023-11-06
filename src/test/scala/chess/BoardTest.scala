package chess

import org.scalatest.funsuite.AnyFunSuite
import chess.pieces._

class BoardFunSuite extends AnyFunSuite {

  test("An empty board should have no moves") {
    val board = Board.empty
    require(board.legalMoves.isEmpty)
    require(board.nextTurn().legalMoves.isEmpty)
  }

}