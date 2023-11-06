package chess.pieces 

import org.scalatest.funsuite.AnyFunSuite

import chess._

class RookTest extends AnyFunSuite:
  test("A rook should have 14 moves on an empty board") {
    val board = Board.empty
    val rook = Rook(Position(3, 3), Color.White)
    val moves = rook.moves(board)
    require(moves.size == 14, s"Rook has ${moves.size} moves")
  }

  test("Rook blocked by tow friendly pawns") {
    val board = Board.emptyWith(Vector(
      Rook(Position("a1"), Color.White),
      Pawn(Position("a2"), Color.White),
      Pawn(Position("b1"), Color.White)
    ))
    val rook = board.get(Position("a1")).get
    val moves = rook.moves(board)
    require(moves.size == 0, s"Rook has ${moves.size} moves")
  }