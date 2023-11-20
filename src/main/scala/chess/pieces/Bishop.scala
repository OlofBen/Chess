package chess.pieces

import chess._

case class Bishop(val position: Position, val color: Color) extends Piece:
  override def moves(board: Board): Iterable[Move] =
    straitMoves(board, position, Bishop.directions, color)

  override def movedTo(to: Position): Piece = 
    Bishop(to, color)
  
  override def isAtStartingPosition: Boolean = 
    position == (if color == Color.White then Position(1, 3) else Position(8, 3)) ||
    position == (if color == Color.White then Position(1, 6) else Position(8, 6))

  override def toString(): String = 
    color match
      case Color.Black => "♗"
      case Color.White => "♝"
  

    

object Bishop:
  val directions = 
      (for rowDelta <- Seq(-1, 1)
          colDelta <- Seq(-1, 1)
      yield (rowDelta, colDelta)).toVector