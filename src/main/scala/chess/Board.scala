package chess 

import chess.pieces._

class Board private (board: Seq[Seq[Option[Piece]]], val turn : Color = Color.White):
  lazy val pieces : Set[Piece] = board.flatten.flatten.toSet
  lazy val isCheckmate = legalMoves.isEmpty && isChecked(turn)
  lazy val isStalemate = legalMoves.isEmpty && !isChecked(turn)

  def nextTurn(): Board = new Board(board, turn.opposite)

  /*
    If outside get returns None
    */
  def get(row : Int, col : Int) : Option[Piece] = 
    if row < 1 || row > 8 || col < 1 || col > 8 then None
    else board(row - 1)(col - 1)
  def get(position: Position): Option[Piece] = get(position.row, position.col)

  def set(row : Int, col : Int, piece : Piece) : Board = 
    val newBoard = board.updated(row - 1, board(row - 1).updated(col - 1, Some(piece)))
    new Board(newBoard, turn)

  def remove(row : Int, col : Int) : Board = 
    val newBoard = board.updated(row - 1, board(row - 1).updated(col - 1, None))
    new Board(newBoard, turn)

  def move(move : Move): Board = 
    val from = move.from
    val to = move.to
    require(isPieceAt(from), "No piece at position")
    val piece = get(from).get
    require(piece.color == turn, "Wrong color")
    require(!isPieceAtWhitColor(to, turn), "Can't take own piece")
    require(get(from).get.moves(this).contains(move), "Illegal move")
    (if piece.isInstanceOf[King] && from.distanceTo(to) == 2 then 
      castleMove(from, to)
    else if piece.isInstanceOf[Pawn] && (to.row == 1 || to.row == 8) then 
      remove(from.row, from.col).set(to.row, to.col, Queen(to, piece.color))
    else
      val piece = get(from.row, from.col).get
      remove(from.row, from.col).set(to.row, to.col, piece.movedTo(to))
    ).nextTurn()

  def move(line: String): Board = 
    val from = Position(line.take(2))
    val to = Position(line.drop(2))
    move(Move(from, to))

  def move(piecePos : (Piece, Position)): Board = 
    move(Move(piecePos._1.position, piecePos._2))

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

  lazy val legalMoves : Set[Move] = 
    pieces.filter(_.color == turn).flatMap ( piece => 
      piece.moves(this)
    ).filter(move => 
      !moveLeadsToCheck(move)
    )
  
  def moveLeadsToCheck(move: Move): Boolean = 
    val newBoard = this.move(move)
    newBoard.isChecked(turn)

  def isChecked(color: Color): Boolean = 
    pieces.exists { piece => 
      piece match
        case king: King if king.color == color => king.isChecked(this)
        case _ => false
    }

  override def toString(): String = 
    board.reverse.map { row => 
      row.map { 
        case Some(piece) => piece.toString
        case None => "."
      }.mkString
    }.mkString("\n")

  override def hashCode(): Int = 
    board.hashCode() - turn.hashCode()

  override def equals(x: Any): Boolean = 
    x match 
      case other : Board => 
        pieces == other.pieces && turn == other.turn
      case _ => false


object Board:
  def apply(board: Seq[Seq[Option[Piece]]]): Board = 
    require(board.size == 8, "Board must have 8 rows")
    require(board.forall(_.size == 8), "Board must have 8 columns")
    new Board(board)

  def empty: Board = 
    val board = Seq.fill(8)(Seq.fill(8)(None))
    Board(board)

  def emptyWith(piece: Seq[Piece]): Board = 
    val board = empty
    piece.foldLeft(board) { (board, piece) => 
      board.set(piece.position.row, piece.position.col, piece)
    }

  def startingPosition() : Board = 
    val pieces = Seq(
      Rook(Position("a1"), Color.White),
      Knight(Position("b1"), Color.White),
      Bishop(Position("c1"), Color.White),
      Queen(Position("d1"), Color.White),
      King(Position("e1"), Color.White),
      Bishop(Position("f1"), Color.White),
      Knight(Position("g1"), Color.White),
      Rook(Position("h1"), Color.White),
      Pawn(Position("a2"), Color.White),
      Pawn(Position("b2"), Color.White),
      Pawn(Position("c2"), Color.White),
      Pawn(Position("d2"), Color.White),
      Pawn(Position("e2"), Color.White),
      Pawn(Position("f2"), Color.White),
      Pawn(Position("g2"), Color.White),
      Pawn(Position("h2"), Color.White),
      Rook(Position("a8"), Color.Black),
      Knight(Position("b8"), Color.Black),
      Bishop(Position("c8"), Color.Black),
      Queen(Position("d8"), Color.Black),
      King(Position("e8"), Color.Black),
      Bishop(Position("f8"), Color.Black),
      Knight(Position("g8"), Color.Black),
      Rook(Position("h8"), Color.Black),
      Pawn(Position("a7"), Color.Black),
      Pawn(Position("b7"), Color.Black),
      Pawn(Position("c7"), Color.Black),
      Pawn(Position("d7"), Color.Black),
      Pawn(Position("e7"), Color.Black),
      Pawn(Position("f7"), Color.Black),
      Pawn(Position("g7"), Color.Black),
      Pawn(Position("h7"), Color.Black),
    )
    emptyWith(pieces)