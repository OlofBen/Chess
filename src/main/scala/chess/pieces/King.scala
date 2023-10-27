package chess.pieces

import chess._

class King(val position: Position, val color: Color) extends Piece:
  def moves(board: Board): Set[Position] = 
    (for 
      rowDelta <- -1 to 1
      colDelta <- -1 to 1
      if !(rowDelta == 0 && colDelta == 0)
      to = position.moved(rowDelta, colDelta)
      if to.isInside && !board.isPieceAtWhitColor(to, color)
    yield to).toSet
    
  def movedTo(to: Position): Piece = 
    King(to, color)

  def isChecked(board: Board): Boolean = 
    board.pieces.exists { piece => 
      piece.color != color && piece.moves(board).contains(position)
    }

  override def toString(): String = 
    color match
      case Color.Black => "♔"
      case Color.White => "♚"