package chess

trait Piece: 
  val position: Position
  val color: Color
  def moves(board : Board): Set[Position]
  def movedTo(to: Position): Piece

  protected def straitMoves(board :Board, startingPosition: Position, directions: Vector[(Int,Int)], colorOfPice: Color): Set[Position] =
    val moves = 
      for (rowDelta, colDelta) <- directions yield 
        LazyList
          .iterate(startingPosition.moved(rowDelta, colDelta))(_.moved(rowDelta, colDelta))
          .takeUntilOrTo(
            Seq(!_.isInside, board.isPieceAtWhitColor(_, colorOfPice)),
            Seq(board.isPieceAtWhitColor(_, colorOfPice.opposite))
          )
    moves.flatten.toSet   


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
  