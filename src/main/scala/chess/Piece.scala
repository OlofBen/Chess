package chess

import chess.pieces._

trait Piece: 
  val position: Position
  val color: Color
  def moves(board : Board): Iterable[Move]
  def movedTo(to: Position): Piece
  def isAtStartingPosition: Boolean

  protected def straitMoves(
    board :Board, startingPosition: Position, 
    directions: Vector[(Int,Int)], 
    colorOfPice: Color
  ): Iterable[Move] =
    val moves : Iterable[Iterable[Move]] = 
      for (rowDelta, colDelta) <- directions yield 
        movesInLine(board, startingPosition, startingPosition, color, rowDelta, colDelta)
    moves.flatten.toVector

  private def movesInLine(
      board : Board, 
      startingPosition: Position,
      current: Position, //Used for recursion
      colorOfPice: Color, 
      rowDelta: Int, 
      colDelta: Int
    ) : Vector[Move] = 
      val nextPosition = current.moved(rowDelta, colDelta)
      if !nextPosition.isInside then Vector.empty
      else if board.isPieceAtWhitColor(nextPosition, colorOfPice) then Vector.empty
      else if board.isPieceAtWhitColor(nextPosition, colorOfPice.opposite) then Vector(Move(startingPosition, nextPosition, isCapture = true))
      else 
        Move(startingPosition, nextPosition)
        +: movesInLine(board, startingPosition, nextPosition, colorOfPice, rowDelta, colDelta)


object Piece: 
  def fromLetter(letter : Char, pos : Position, color : Color) = 
    letter.toLower match
      case 'p' => Pawn(pos, color)
      case 'r' => Rook(pos, color)
      case 'n' => Knight(pos, color)
      case 'b' => Bishop(pos, color)
      case 'q' => Queen(pos, color)
      case 'k' => King(pos, color)
      case _ => throw new Exception("Invalid piece letter")
    
  
  