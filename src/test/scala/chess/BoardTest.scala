package chess

import org.scalatest.funsuite.AnyFunSuite
import chess.pieces._

class BoardFunSuite extends AnyFunSuite {

  test("An empty board should have no moves") {
    val board = Board.empty
    require(board.legalMoves(Color.White).isEmpty)
    require(board.legalMoves(Color.Black).isEmpty)
  }

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

  test("Rook blocked by tow enemy pawns") {
    val board = Board.emptyWith(Vector(
      Rook(Position("a1"), Color.White),
      Pawn(Position("a2"), Color.Black),
      Pawn(Position("b1"), Color.Black)
    ))
    val rook = board.get(Position("a1")).get
    val moves = rook.moves(board)
    require(moves.size == 2, s"Rook has ${moves.size} moves")
  }

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

  test("King can't take own piece") {
    val board = Board.emptyWith(Vector(
      King(Position("d4"), Color.White),
      Pawn(Position("d5"), Color.White)
    ))
    val king = board.get(Position("d4")).get
    val moves = king.moves(board)
    require(moves.size == 7, s"King has ${moves.size} moves")
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

  test("Pawn can two squares first but then only one") {
    val board = Board.emptyWith(Vector(
      Pawn(Position("d2"), Color.White)
    ))
    val pawn = board.get(Position("d2")).get
    val moves = pawn.moves(board)
    require(moves.size == 2, s"Pawn has ${moves.size} moves")
    require(pawn.movedTo(Position("d4")).moves(board) == Set(Position("d5")))
  }


}