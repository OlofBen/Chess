package chess.pieces

import chess._

case class King(val position: Position, val color: Color, hasMoved:Boolean = false) extends Piece:
  def moves(board: Board): Iterable[Move] = 
    (for 
      rowDelta <- -1 to 1
      colDelta <- -1 to 1
      if !(rowDelta == 0 && colDelta == 0)
      to = position.moved(rowDelta, colDelta)
      if to.isInside && !board.isPieceAtWhitColor(to, color)
    yield Move(position, to, isCapture = board.isPieceAtWhitColor(to, color.opposite))
    ).toVector ++ castleMoves(board)
    
  def castleMoves(board: Board): Iterable[Move] = 
    if hasMoved || isChecked(board) then Vector.empty
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
        Move(position, Position(position.row, newKingCol), isCastle = true)
      }.toVector
    
  def movedTo(to: Position): Piece = 
    King(to, color, hasMoved=true)

  def isChecked(board: Board): Boolean =    
      isCheckedByKnight(board)        || 
      isCheckedByPawn(board)          || 
      isCheckedByRookOrQueen(board)   || 
      isCheckedByBishopOrQueen(board) || 
      isCheckedByKing(board)      

  def isCheckedByKnight(board : Board) = 
    Knight.directions.map(position.moved).flatMap(board.get(_))
      .exists(_ match 
        case knight: Knight if knight.color != color => true
        case _ => false
      )
  def isCheckedByPawn(board : Board) = 
    (color match
      case Color.White => Seq(position.moved(1, -1), position.moved(1, 1))
      case Color.Black => Seq(position.moved(-1, -1), position.moved(-1, 1))
    ).flatMap(board.get(_))
    .exists(_ match 
      case pawn: Pawn if pawn.color != color => true
      case _ => false
    )

  def isCheckedByRookOrQueen(board : Board) = 
    Rook.directions
      .flatMap(dir => 
        LazyList.iterate(position.moved(dir))(pos => pos.moved(dir))
          .takeWhile(_.isInside)
          .flatMap(board.get(_))
          .headOption
      ).exists(_ match 
          case rook: Rook if rook.color != color => true
          case queen: Queen if queen.color != color => true
          case _ => false
      )
  def isCheckedByBishopOrQueen(board : Board) = 
    Bishop.directions
      .flatMap(
        dir => 
          LazyList.iterate(position.moved(dir))(pos => pos.moved(dir))
            .takeWhile(_.isInside)
            .flatMap(board.get(_))
            .headOption)
          .exists(_ match 
              case bishop: Bishop if bishop.color != color => true
              case queen: Queen if queen.color != color => true
              case _ => false
            )
  def isCheckedByKing(board : Board)= 
    King.directions
      .map(position.moved)
      .flatMap(board.get(_))
      .exists(_ match 
        case king: King if king.color != color => true
        case _ => false
      )


  override def toString(): String = 
    color match
      case Color.Black => "♔"
      case Color.White => "♚"

object King: 
  val directions: Vector[(Int, Int)] = 
    (for 
      rowDelta <- -1 to 1
      colDelta <- -1 to 1
      if !(rowDelta == 0 && colDelta == 0)
    yield (rowDelta, colDelta)).toVector