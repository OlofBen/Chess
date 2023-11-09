package chess.engine.static_eval.simplified_evaluation_function

import chess._
import chess.pieces._
import chess.engine.static_eval._

object Simplified_Evaluation_Function extends StaticEvaluator:
  def evaluate(board: Board): Int =
    if board.isCheckmate then 
      board.turn match
        case Color.White => Int.MinValue
        case Color.Black => Int.MaxValue
    else if board.isStalemate then 
      0
    else
      val pieces = board.pieces
      val scorePerPiece = pieces.map(piece => score(piece) + PieceSquareTable(piece))
      val scorePiceColor = scorePerPiece.zip(pieces.map(_.color)).map((score, color) => score * (if color == Color.White then 1 else -1))
      scorePiceColor.sum

    
  
  def score(piece : Piece):Int = //https://www.chessprogramming.org/Simplified_Evaluation_Function
    piece match
      case _: King => 20000
      case _: Queen => 900
      case _: Rook => 500
      case _: Bishop => 330
      case _: Knight => 320
      case _: Pawn => 100



