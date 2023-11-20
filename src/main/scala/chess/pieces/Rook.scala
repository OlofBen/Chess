package chess.pieces

import chess._

case class Rook(val position: Position, val color: Color, val hasMoved: Boolean = false) extends Piece:
  override def moves(board: Board): Iterable[Move] =
    straitMoves(board, position, Rook.directions, color) 
    
  override def movedTo(to: Position): Piece = 
    Rook(to, color, true)

  override def isAtStartingPosition = 
    position == (if color == Color.White then Position(1, 1) else Position(8, 1)) ||
    position == (if color == Color.White then Position(1, 8) else Position(8, 8))

  override def toString(): String = 
    color match
      case Color.Black => "♖"
      case Color.White => "♜"

  
object Rook:
  val directions = Vector((1, 0), (-1, 0), (0, 1), (0, -1))