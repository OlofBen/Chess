package chess.pieces

import chess._

case class Rook(val position: Position, val color: Color, val hasMoved: Boolean = false) extends Piece:
  def moves(board: Board): Iterable[Move] =
    straitMoves(board, position, Rook.directions, color) 
    
  def movedTo(to: Position): Piece = 
    Rook(to, color, true)

  override def toString(): String = 
    color match
      case Color.Black => "♖"
      case Color.White => "♜"

  
object Rook:
  val directions = Vector((1, 0), (-1, 0), (0, 1), (0, -1))