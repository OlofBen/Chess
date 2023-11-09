package chess 

import chess.pieces._

case class Board private (board: Vector[Vector[Option[Piece]]], val turn : Color = Color.White, enPassantSquare : Option[Position] = None):
  lazy val pieces : Seq[Piece] = board.flatten.flatten
  lazy val isCheckmate = legalMoves.isEmpty && isChecked(turn)
  lazy val isStalemate = legalMoves.isEmpty && !isChecked(turn)
  lazy val kings = pieces.collect({ case king: King => king })
  lazy val whiteIsChecked = kings.find(_.color == Color.White).exists(_.isChecked(this))
  lazy val blackIsChecked = kings.find(_.color == Color.Black).exists(_.isChecked(this))

  def nextTurn(enPassantSquare : Option[Position] = None): Board = 
    this.copy(turn = turn.opposite, enPassantSquare = enPassantSquare)
  

  /*
    If outside get returns None
    */
  def get(row : Int, col : Int) : Option[Piece] = 
    if row < 1 || row > 8 || col < 1 || col > 8 then None
    else board(row - 1)(col - 1)
  def get(position: Position): Option[Piece] = get(position.row, position.col)

  def set(row : Int, col : Int, piece : Option[Piece]) : Board = 
    val newBoard = board.updated(row - 1, board(row - 1).updated(col - 1, piece))
    this.copy(board = newBoard)

  def set(row : Int, col : Int, piece : Piece) : Board = 
    set(row, col, Some(piece))

  def remove(row : Int, col : Int) : Board = 
    set(row, col, None)

  def move(move : Move): Board = 
    val from = move.from
    val to = move.to
    require(isPieceAt(from), s"No piece at position $from")
    val piece = get(from).get
    require(piece.color == turn, "Wrong color")
    require(!isPieceAtWhitColor(to, turn), "Can't take own piece")
    require(get(from).get.moves(this).exists(_ == move), "Illegal move")

    val enPassantSquare = if move.isPawnMovingTwo then Some(from.halfwayTo(to)) else None

    (if move.isCastle then 
      castleMove(from, to)
    else if move.promotionPiece.isDefined then 
      remove(from.row, from.col).set(to.row, to.col,  move.promotionPiece.get)
    else if move.isEnPassantCapture then 
      remove(from.row, to.col).remove(from.row, from.col).set(to.row, to.col, piece.movedTo(to))
    else
      remove(from.row, from.col).set(to.row, to.col, piece.movedTo(to))
    ).nextTurn(enPassantSquare)

  def move(line: String): Board = 
    val from = Position(line.take(2))
    val to = Position(line.drop(2).take(2))//need take two in case of promotion
    var move = Move(from, to)
    if get(from).get.isInstanceOf[King] && from.distanceTo(to) == 2 then 
      move = move.copy(isCastle = true) 
    else if line.size == 5 then 
      val newPiece = Piece.fromLetter(line.last, to, turn)
      move = move.copy(promotionPiece = Some(newPiece))
    
    this.move(move)

  def castleMove(from: Position, to: Position): Board = 
    require(get(from).get.isInstanceOf[King], "Can't castle with non king")
    val king = get(from).get
    val rookCol = if to.col == 3 then 1 else 8
    
    require(get(to.row, rookCol).get.isInstanceOf[Rook], "Can't castle with non rook")
    val rook = get(to.row, rookCol).get
    val newRookCol = if to.col == 3 then 4 else 6
    remove(from.row, from.col)
      .remove(from.row, rookCol)
      .set(to.row, to.col, king.movedTo(to))
      .set(to.row, newRookCol, rook.movedTo(Position(to.row, newRookCol)))

  def isPieceAt(position: Position): Boolean = 
    get(position.row, position.col).isDefined

  def isPieceAtWhitColor(position: Position, color: Color): Boolean = 
    get(position.row, position.col).exists(_.color == color)

  lazy val legalMoves : Seq[Move] = 
    pieces.filter(_.color == turn).flatMap ( piece => 
      piece.moves(this)
    ).filter(move => 
      !moveLeadsToCheck(move)
    )
  
  def moveLeadsToCheck(move: Move): Boolean = 
    val newBoard = this.move(move)
    newBoard.isChecked(turn)

  def isChecked(color: Color): Boolean = 
    color.match
      case Color.White => whiteIsChecked
      case Color.Black => blackIsChecked

  def withEnPassant(position: Position): Board = 
    this.copy(enPassantSquare = Some(position))
    
  override def toString(): String = 
    board.reverse.map { row => 
      row.map { 
        case Some(piece) => piece.toString
        case None => "."
      }.mkString(" ")
    }.mkString("\n")



object Board:
  def apply(board: Vector[Vector[Option[Piece]]]): Board = 
    require(board.size == 8, "Board must have 8 rows")
    require(board.forall(_.size == 8), "Board must have 8 columns")
    new Board(board)

  def empty: Board = 
    val board = Vector.fill(8, 8)(None)
    Board(board)

  def emptyWith(piece: Seq[Piece]): Board = 
    val board = empty
    piece.foldLeft(board) { (board, piece) => 
      board.set(piece.position.row, piece.position.col, piece)
    }

  def fromFen(fen: String): Board = 
    val boardState = fen.split(" ").head
    val rows = boardState.split("/").toVector
    val board = rows.zipWithIndex.map { (row, rowNumber) => 
      var colNumber = 0 // 0-indexed
      row.flatMap { 
        case c if c.isDigit => 
          val n = c.asDigit
          colNumber += n
          Vector.fill(n)(None)
        case c => 
          colNumber += 1
          val piece = Piece.fromLetter(c, Position(8 - rowNumber, colNumber), if c.isUpper then Color.White else Color.Black)
          Vector(Some(piece))
      }.toVector
    }.reverse
    Board(board)

  def startingPosition() : Board = 
    fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")