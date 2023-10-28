package chess

import chess.pieces._

object Evaluation:
  def evaluate(board: Board): Int =
    val whitePieces = board.pieces.filter(_.color == Color.White)
    val blackPieces = board.pieces.filter(_.color == Color.Black)
    val whiteScore = whitePieces.map(score).sum
    val blackScore = blackPieces.map(score).sum
    whiteScore - blackScore
  
  def score(piece : Piece):Int = 
    piece match
      case _: King => 1000000
      case _: Queen => 9
      case _: Rook => 5
      case _: Bishop => 3
      case _: Knight => 3
      case _: Pawn => 1


