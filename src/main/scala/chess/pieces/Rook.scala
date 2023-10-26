package chess.pieces

import chess._

class Rook(val position: Position, val color: Color) extends Piece:
  def moves(board: Board): Set[Position] =
    straitMoves(board, position, Rook.directions, color) 
    
  def movedTo(to: Position): Piece = 
    Rook(to, color)

  override def toString(): String = 
    color match
      case Color.Black => "♖"
      case Color.White => "♜"

  
object Rook:
  val directions = Vector((1, 0), (-1, 0), (0, 1), (0, -1))