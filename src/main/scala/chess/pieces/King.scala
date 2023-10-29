package chess.pieces

import chess._

class King(val position: Position, val color: Color, hasMoved:Boolean = false) extends Piece:
  def moves(board: Board): Set[Move] = 
    (for 
      rowDelta <- -1 to 1
      colDelta <- -1 to 1
      if !(rowDelta == 0 && colDelta == 0)
      to = position.moved(rowDelta, colDelta)
      if to.isInside && !board.isPieceAtWhitColor(to, color)
    yield Move(position, to)
    ).toSet ++ castleMoves(board)

  def castleMoves(board: Board): Set[Move] = 
    if hasMoved || isChecked(board) then Set.empty
    else 
      board.pieces.collect {
        case rook: Rook 
          if rook.color == color 
          && !rook.hasMoved 
          && rook.position.row == position.row 
          && ((rook.position.col == 1 && (2 to 4).forall(col => !board.isPieceAt(Position(position.row, col)))) ||
              (rook.position.col == 8 && (6 to 7).forall(col => !board.isPieceAt(Position(position.row, col)))))
          => rook 
      }.map { rook => 
        val newKingCol = if rook.position.col == 1 then 3 else 7
        Move(position, Position(position.row, newKingCol))
      }.toSet
    
  def movedTo(to: Position): Piece = 
    King(to, color, hasMoved=true)

  def isChecked(board: Board): Boolean = 
    board.pieces.filterNot(_.isInstanceOf[King]).exists { piece => //Cant loop over legal moves, that would be infinite loop
      piece.color != color && piece.moves(board).exists(_.to ==position)
    }

  override def toString(): String = 
    color match
      case Color.Black => "♔"
      case Color.White => "♚"