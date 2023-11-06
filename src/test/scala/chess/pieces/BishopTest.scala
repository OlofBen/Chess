package chess.pieces

import org.scalatest.funsuite.AnyFunSuite

import chess._

class BishopTest extends AnyFunSuite:
  test("Bishop in middle") {
    val board = Board.emptyWith(Vector(
      Bishop(Position("d4"), Color.White)
    ))
    val bishop = board.get(Position("d4")).get
    val moves = bishop.moves(board)
    require(moves.size == 13, s"Bishop has ${moves.size} moves")
  }

  test("Bishop in corner") {
    val board = Board.emptyWith(Vector(
      Bishop(Position("a1"), Color.White)
    ))
    val bishop = board.get(Position("a1")).get
    val moves = bishop.moves(board)
    require(moves.size == 7, s"Bishop has ${moves.size} moves")
  }

  test("Bishop can't take own piece") {
    val board = Board.emptyWith(Vector(
      Bishop(Position("d4"), Color.White),
      Pawn(Position("e5"), Color.White)
    ))
    val bishop = board.get(Position("d4")).get
    val moves = bishop.moves(board)
    require(moves.size == 9, s"Bishop has ${moves.size} moves")
  }

  test("Bishop can take opponent piece") {
    val board = Board.emptyWith(Vector(
      Bishop(Position("d4"), Color.White),
      Pawn(Position("e5"), Color.Black)
    ))
    val bishop = board.get(Position("d4")).get
    val moves = bishop.moves(board)
    require(moves.size == 10, s"Bishop has ${moves.size} moves")
    require(moves.filter(_.isCapture).size == 1, s"Bishop has ${moves.filter(_.isCapture).size} capture moves")
  }

