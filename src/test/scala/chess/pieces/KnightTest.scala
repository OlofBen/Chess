package chess.pieces

import org.scalatest.funsuite.AnyFunSuite

import chess._

class KnightTest extends AnyFunSuite:
  test("Knight in middle") {
    val board = Board.emptyWith(Vector(
      Knight(Position("d4"), Color.White)
    ))
    val knight = board.get(Position("d4")).get
    val moves = knight.moves(board)
    require(moves.size == 8, s"Knight has ${moves.size} moves")
  }

  test("Knight in corner") {
    val board = Board.emptyWith(Vector(
      Knight(Position("a1"), Color.White)
    ))
    val knight = board.get(Position("a1")).get
    val moves = knight.moves(board)
    require(moves.size == 2, s"Knight has ${moves.size} moves")
  }

  test("Knight can't take own piece") {
    val board = Board.emptyWith(Vector(
      Knight(Position("d4"), Color.White),
      Pawn(Position("e6"), Color.White)
    ))
    val knight = board.get(Position("d4")).get
    val moves = knight.moves(board)
    require(moves.size == 7, s"Knight has ${moves.size} moves")
  }
