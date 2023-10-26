package chess.pieces

import chess._

class Knight(val position: Position, val color: Color) extends Piece:
  def moves(board: Board): Set[Position] =
    (for (rowDelta, colDelta) <- Knight.directions yield 
      position.moved(rowDelta, colDelta)
    ).filter(_.isInside).toSet
              
  def movedTo(to: Position): Piece = 
    Knight(to, color)

  override def toString(): String = 
    color match
      case Color.Black => "♘"
      case Color.White => "♞"

object Knight:
  val directions = 
      for rowDelta <- Seq(-2, -1, 1, 2)
          colDelta <- Seq(-2, -1, 1, 2)
          if rowDelta.abs != colDelta.abs
      yield (rowDelta, colDelta)