package chess.pieces

import chess._

class Bishop(val position: Position, val color: Color) extends Piece:
  def moves(board: Board): Set[Position] =
    straitMoves(board, position, Bishop.directions, color)
  def movedTo(to: Position): Piece = 
    Bishop(to, color)

  override def toString(): String = 
    color match
      case Color.Black => "♗"
      case Color.White => "♝"
    

object Bishop:
  val directions = 
      (for rowDelta <- Seq(-1, 1)
          colDelta <- Seq(-1, 1)
      yield (rowDelta, colDelta)).toVector