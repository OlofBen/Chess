package chess.pieces

import org.scalatest.funsuite.AnyFunSuite

import chess._


class QueenTest extends AnyFunSuite:
  test("Queen in the middle") {
    val board = Board.emptyWith(Vector(
      Queen(Position("d4"), Color.White)
    ))
    val queen = board.get(Position("d4")).get
    val moves = queen.moves(board)
    require(moves.size == 27, s"Queen has ${moves.size} moves")
  }
  test("Queen in the corner") {
    val board = Board.emptyWith(Vector(
      Queen(Position("a1"), Color.White)
    ))
    val queen = board.get(Position("a1")).get
    val moves = queen.moves(board)
    require(moves.size == 21, s"Queen has ${moves.size} moves")
  }