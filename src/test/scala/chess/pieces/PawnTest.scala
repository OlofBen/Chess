package chess.pieces

import org.scalatest.funsuite.AnyFunSuite

import chess._

class PawnTest extends AnyFunSuite:
  test("Pawn can two squares first but then only one") {
    val board = Board.emptyWith(Vector(
      Pawn(Position("d2"), Color.White)
    ))
    val pawn = board.get(Position("d2")).get
    val moves = pawn.moves(board)
    require(moves.size == 2, s"Pawn has ${moves.size} moves")
    require(pawn.movedTo(Position("d4")).moves(board).forall(m => Set(Move("d4", "d5")).contains(m)), "Pawn can't move two squares, if it has moved")
  }
  
  test("Pawn can take piece"){
    val board = Board.emptyWith(Vector(
      Pawn(Position("d2"), Color.White),
      Pawn(Position("c3"), Color.Black)
    ))
    val pawn = board.get(Position("d2")).get
    val moves = pawn.moves(board)
    require(moves.size == 3, s"Pawn has ${moves.size} moves")
    require(moves.filter(_.isCapture).size == 1, s"Pawn has ${moves.filter(_.isCapture).size} capture moves")
  }
 
