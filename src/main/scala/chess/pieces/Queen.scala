package chess.pieces

import chess._

class Queen(val position: Position, val color: Color) extends Piece:
  def moves(board: Board): Set[Position] =
    straitMoves(board, position, Queen.directions, color) 
  def movedTo(to: Position): Piece = 
    Queen(to, color)

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