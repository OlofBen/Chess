package chess

import org.scalatest.funsuite.AnyFunSuite
import chess.pieces._

class TranslatorFunSuite extends AnyFunSuite:
  test("Pawn to E4") {
    val board = Board.startingPosition()
    val newBoard = board.moveAlgebraicNotation("e4")
    require(newBoard.get(Position("e4")).isDefined)
  }

  test("Three queens can move to same position") {
    val board = Board.emptyWith(Vector(
      Queen(Position("h1"), Color.White),
      Queen(Position("h4"), Color.White),
      Queen(Position("e4"), Color.White)
    ))
    val newBoard = board.moveAlgebraicNotation("Qh4e1")
    require(newBoard.get(Position("e1")).isDefined)
    require(newBoard.get(Position("h4")).isEmpty)
    require(newBoard.get(Position("e4")).isDefined)
    require(newBoard.get(Position("h1")).isDefined)
  }

  test("Two rooks on same column can move to same position") {
    val board = Board.emptyWith(Vector(
      Rook(Position("a1"), Color.White),
      Rook(Position("a5"), Color.White)
    ))
    val newBoard = board.moveAlgebraicNotation("R1a3")
    require(newBoard.get(Position("a5")).isDefined)
    require(newBoard.get(Position("a3")).isDefined)
    require(newBoard.get(Position("a1")).isEmpty)
  }

    test("Two rooks on same row can move to same position") {
    val board = Board.emptyWith(Vector(
      Rook(Position("d8"), Color.White),
      Rook(Position("h8"), Color.White)
    ))
    val newBoard = board.moveAlgebraicNotation("Rdf8")
    require(newBoard.get(Position("h8")).isDefined)
    require(newBoard.get(Position("f8")).isDefined)
    require(newBoard.get(Position("d8")).isEmpty)
  }
