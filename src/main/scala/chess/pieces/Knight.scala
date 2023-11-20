package chess.pieces

import chess._

case class Knight(val position: Position, val color: Color) extends Piece:
  override def moves(board: Board): Iterable[Move] =
    (for (rowDelta, colDelta) <- Knight.directions yield 
      position.moved(rowDelta, colDelta)
    ).filter(_.isInside)
      .filter(!board.isPieceAtWhitColor(_, color))
      .map(to => Move(position, to, isCapture = board.isPieceAtWhitColor(to, color.opposite)))
              
  override def movedTo(to: Position): Piece = 
    Knight(to, color)

  override def isAtStartingPosition = 
    position == (if color == Color.White then Position(1, 2) else Position(8, 2)) ||
    position == (if color == Color.White then Position(1, 7) else Position(8, 7))

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