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

  test("Pawn can en passant") {
    val board = Board.emptyWith(Vector(
      Pawn(Position("d5"), Color.White, hasMoved = true),
      Pawn(Position("c5"), Color.Black, hasMoved = true)
    )).withEnPassant(Position("c6"))
    val pawn = board.get(Position("d5")).get
    val moves = pawn.moves(board)
    require(moves.size == 2, s"Pawn has ${moves.size} moves: $moves")
    require(moves.filter(_.isEnPassantCapture).size == 1, s"Pawn has ${moves.filter(_.isEnPassantCapture).size} en passant capture moves")
  }

  test("Pre programed pos 2") {
    val board = Board.fromFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ")
      .move("a2a4")

    require(board.legalMoves.toSet.contains(Move("b4","a3")), "Pawn can't take en passant")
  }
 
