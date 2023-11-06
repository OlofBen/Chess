package chess.engine

import chess._
import chess.pieces._

object Evaluation:
  def evaluate(board: Board): Int =
    if board.isCheckmate then 
      board.turn match
        case Color.White => Int.MinValue
        case Color.Black => Int.MaxValue
    else if board.isStalemate then 
      0
    else
      val whitePieces = board.pieces.filter(_.color == Color.White)
      val blackPieces = board.pieces.filter(_.color == Color.Black)
      val whiteScore = whitePieces.toSeq.map(score).sum
      val blackScore = blackPieces.toSeq.map(score).sum
      whiteScore - blackScore

    
  
  def score(piece : Piece):Int = 
    piece match
      case _: King => 1000000
      case _: Queen => 9
      case _: Rook => 5
      case _: Bishop => 3
      case _: Knight => 3
      case _: Pawn => 1


