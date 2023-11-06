package chess

import chess.pieces._

trait Piece: 
  val position: Position
  val color: Color
  def moves(board : Board): Iterable[Move]
  def movedTo(to: Position): Piece

  protected def straitMoves(
    board :Board, startingPosition: Position, 
    directions: Vector[(Int,Int)], 
    colorOfPice: Color
  ): Iterable[Move] =
    val moves = 
      for (rowDelta, colDelta) <- directions yield 
        LazyList
          .iterate(startingPosition.moved(rowDelta, colDelta))(_.moved(rowDelta, colDelta))
          .takeUntilOrTo(
            Seq(!_.isInside, board.isPieceAtWhitColor(_, colorOfPice)),
            Seq(board.isPieceAtWhitColor(_, colorOfPice.opposite))
          )
    moves.flatten.map(pos => Move(startingPosition, pos)) 


  extension (xs: Seq[Position]) 
    def takeUntilOrTo(until: Seq[Position => Boolean], to: Seq[Position => Boolean]): Seq[Position] = 
      var result = Vector.empty[Position]
      val iterator = xs.iterator
      var quit = false
      while iterator.hasNext && !quit do 
        val head = iterator.next()
        if until.exists(_(head)) then 
          quit = true
        else if to.exists(_(head)) then 
          result :+= head
          quit = true
        else 
          result :+= head
      result


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
    
  
  