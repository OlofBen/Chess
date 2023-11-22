package chess.pieces

import org.scalatest.funsuite.AnyFunSuite

import chess._

class KingTest extends AnyFunSuite:
  test("King can't take own piece") {
    val board = Board.emptyWith(Vector(
      King(Position("d4"), Color.White),
      Pawn(Position("d5"), Color.White)
    ))
    val king = board.get(Position("d4")).get
    val moves = king.moves(board)
    require(moves.size == 7, s"King has ${moves.size} moves")
  }


  // CASTLING
  test("Castling white king side") {
    val king = King(Position("e1"), Color.White)
    val board = Board.emptyWith(Vector(
      king,
      Rook(Position("h1"), Color.White)
    ))
    require(king.moves(board).toSet.contains(Move("e1","g1")), "King can't castle")
    val newBoard = board.move("e1g1")
    require(newBoard.get(Position("g1")).isDefined, "King is not in g1")
    require(newBoard.get(Position("f1")).isDefined, "Rook is not in f1")
    require(newBoard.get(Position("e1")).isEmpty)
    require(newBoard.get(Position("h1")).isEmpty)
  }

  test("Castling white queen side") {
    val board = Board.emptyWith(Vector(
      King(Position("e1"), Color.White),
      Rook(Position("a1"), Color.White)
    ))
    val newBoard = board.move("e1c1")
    require(newBoard.get(Position("c1")).isDefined, "King is not in c1")
    require(newBoard.get(Position("d1")).isDefined, "Rook is not in d1")
    require(newBoard.get(Position("e1")).isEmpty)
    require(newBoard.get(Position("a1")).isEmpty)
  }

  test("Castling black king side") {
    val board = Board.emptyWith(Vector(
      King(Position("e8"), Color.Black),
      Rook(Position("h8"), Color.Black)
    )).nextTurn()
    val newBoard = board.move("e8g8")
    require(newBoard.get(Position("g8")).isDefined, "King is not in g8")
    require(newBoard.get(Position("f8")).isDefined, "Rook is not in f8")
    require(newBoard.get(Position("e8")).isEmpty)
    require(newBoard.get(Position("h8")).isEmpty)
  }

  test("Castling black queen side") {
    val board = Board.emptyWith(Vector(
      King(Position("e8"), Color.Black),
      Rook(Position("a8"), Color.Black)
    )).nextTurn()
    val newBoard = board.move("e8c8")
    require(newBoard.get(Position("c8")).isDefined, "King is not in c8")
    require(newBoard.get(Position("d8")).isDefined, "Rook is not in d8")
    require(newBoard.get(Position("e8")).isEmpty)
    require(newBoard.get(Position("a8")).isEmpty)
  }

  test("King can not turn in to Queen"){ // Old bug
    val board = Board.emptyWith(Vector(
      King(Position("e8"), Color.Black),
    )).nextTurn()
    val newBoard = board.move("e8d8")
    require(newBoard.get(Position("d8")).isDefined, "King is not in d8")
    require(newBoard.get(Position("e8")).isEmpty)
    require(newBoard.get(Position("d8")).get.isInstanceOf[King], "Is not a king")
  }

  test("King taking") {
    val board = Board.emptyWith(Vector(
      King(Position("e8"), Color.Black),
      Pawn(Position("e7"), Color.White)
    )).nextTurn()
    val moves = board.get(Position("e8")).get.moves(board)
    val takingMoves = moves.filter(_.isCapture)
    require(takingMoves.size == 1, s"King has ${takingMoves.size} taking moves")
  }

  test("King can't castle if d1 is threatend"){
    val board = Board.emptyWith(Vector(
      King(Position("e1"), Color.White),
      Rook(Position("a1"), Color.White),
      Rook(Position("d5"), Color.Black)
    ))
    val king = board.get(Position("e1")).get
    val moves = king.moves(board)
    require(!moves.toSet.contains(Move("e1","c1")), "King can castle")
  }

  test("King cant castle if checked") {
    val board = Board.emptyWith(Vector(
      King(Position("e1"), Color.White),
      Rook(Position("a1"), Color.White),
      Rook(Position("e5"), Color.Black)
    ))
    val king = board.get(Position("e1")).get
    val moves = king.moves(board)
    require(!moves.toSet.contains(Move("e1","g1")), "King can castle")
  }
