package chess.pieces

import chess._

case class Pawn(val position: Position, val color: Color, hasMoved:Boolean = false) extends Piece:
  def moves(board: Board): Set[Move] =
    val direction = color match
      case Color.White => 1
      case Color.Black => -1
    val oneForward = position.moved(rowDelta = direction)
    val twoForward = position.moved(rowDelta = 2 * direction)
    val forwardMoves = 
      if board.isPieceAt(oneForward) then Set.empty
      else if hasMoved || board.isPieceAt(twoForward) then Set(oneForward)
      else Set(oneForward, twoForward)
    val diagonalMoves = 
      Set(position.moved(direction, - 1),
          position.moved(direction, + 1))
        .filter(board.isPieceAtWhitColor(_, color.opposite))
    (forwardMoves ++ diagonalMoves).filter(_.isInside).map(to => Move(position, to)).toSet
  def movedTo(to: Position): Piece = 
    Pawn(to, color, hasMoved = true)
  def movedToAndPromotedTo(to: Position, piece: Piece): Piece = 
    piece.movedTo(to)

  override def toString(): String = 
    color match
      case Color.Black => "♙"
      case Color.White => "♟︎"
      