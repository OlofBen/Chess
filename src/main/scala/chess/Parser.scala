package chess 

import chess.pieces._

object Parser:
  /**
    * Translates from algebraic notation to position
    */
  def parse(algebraicNotation: String, pieces : Set[Piece], board : Board): (Position, Position) = 
    if algebraicNotation == "0-0" then ??? //Kingside Castle
    else if algebraicNotation == "0-0-0" then ??? //Queenside Castle
    
    lazy val filterFunction = getPiece(algebraicNotation)
    lazy val possiblePieces = pieces.filter(filterFunction)

    //Not used yet
    lazy val captures = algebraicNotation.contains("x")
    lazy val check = algebraicNotation.contains("+")
    lazy val checkmate = algebraicNotation.contains("#")
    // TODO: Promotion
    
    val move = algebraicNotation.filterNot(c => c == 'x' || c == '+' || c == '#' || c.isUpper)
    val to = Position(move.takeRight(2))
  
    if move.size == 2 then 
      val movingPiece = possiblePieces.find(piece => piece.moves(board).contains(to)).get
      (movingPiece.position, to)
    else 
      val starting = move.dropRight(2)
      val rowOption = starting.filter(_.isDigit).toIntOption
      val colOption = starting.filter(_.isLetter).map(_.toInt - 96).headOption
      if rowOption.isDefined && colOption.isDefined then 
        val row = rowOption.get
        val col = colOption.get
        val movingPiece = possiblePieces.find(piece => piece.position.row == row && piece.position.col == col).get
        (movingPiece.position, to)
      else if rowOption.isDefined then 
        val row = rowOption.get
        val movingPiece = possiblePieces.find(piece => piece.position.row == row && piece.moves(board).contains(to)).get
        (movingPiece.position, to)
      else  
        require(colOption.isDefined, s"Unexpected move: $algebraicNotation")
        val col = colOption.get
        val movingPiece = possiblePieces.find(piece => piece.position.col == col && piece.moves(board).contains(to)).get
        (movingPiece.position, to)

          
        

    

  def getPiece(algebraicNotation: String): Piece => Boolean = 
    algebraicNotation.head match
      case 'K' => _.isInstanceOf[King]
      case 'Q' => _.isInstanceOf[Queen]
      case 'R' => _.isInstanceOf[Rook]
      case 'B' => _.isInstanceOf[Bishop]
      case 'N' => _.isInstanceOf[Knight]
      case  _  => _.isInstanceOf[Pawn]