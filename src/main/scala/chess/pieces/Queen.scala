package chess.pieces

import chess._

case class Queen(val position: Position, val color: Color) extends Piece:
  override def moves(board: Board): Iterable[Move] =
    straitMoves(board, position, Queen.directions, color) 
    
  override def movedTo(to: Position): Piece = 
    Queen(to, color)

  override def isAtStartingPosition: Boolean = 
    position == (if color == Color.White then Position(1, 4) else Position(8, 4))

  override def toString(): String = 
    color match
      case Color.Black => "♕"
      case Color.White => "♛"

object Queen:
  val directions = 
    (for rowDelta <- Seq(-1, 0, 1)
        colDelta <- Seq(-1, 0, 1)
        if !(rowDelta == 0 && colDelta == 0)
    yield (rowDelta, colDelta)).toVector