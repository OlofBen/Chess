package chess.pieces

import chess._

case class Pawn(val position: Position, val color: Color, hasMoved:Boolean = false) extends Piece:
  def moves(board: Board): Iterable[Move] = // Todo: En passant
    val direction = color match
      case Color.White => 1
      case Color.Black => -1
    
    val diagonalMoves = 
      Vector(position.moved(direction, - 1),
          position.moved(direction, + 1))
        .filter(board.isPieceAtWhitColor(_, color.opposite))
        .map(to => Move(position, to, isCapture = true))
    (forwardMoves(board, direction) ++ diagonalMoves)
      .filter(move => move.to.isInside)

  def forwardMoves(board: Board, direction: Int): Iterable[Move] = 
    lazy val oneForward = position.moved(rowDelta = direction)
    lazy val twoForward = position.moved(rowDelta = 2 * direction)
    if board.isPieceAt(oneForward) then Vector.empty
    else if !hasMoved && !board.isPieceAt(twoForward) then Vector(oneForward, twoForward).map(to => Move(position, to))
    else 
      if isPromotion(oneForward) then 
        promotionMoves(oneForward) 
      else Vector(Move(position, oneForward))

 
    
  def isPromotion(newPos : Position) : Boolean = 
    color match
      case Color.White => newPos.row == 8
      case Color.Black => newPos.row == 1
    
  def promotionMoves(newPos : Position): Iterable[Move] = 
    val promotionPieces = Vector(Queen(newPos, color), Rook(newPos, color), Bishop(newPos, color), Knight(newPos, color))
    promotionPieces.map(piece => 
      Move(position, newPos, promotionPiece = Some(piece))
    )
    

  def movedTo(to: Position): Piece = 
    Pawn(to, color, hasMoved = true)

  def movedToAndPromotedTo(to: Position, piece: Piece): Piece = 
    piece.movedTo(to)

  override def toString(): String = 
    color match
      case Color.Black => "♙"
      case Color.White => "♟︎"
      