package chess.engine.eval

import chess._
import chess.pieces._

object Evaluation:
  private var evaluatePositions = Map.empty[Board, Int]
  private var tableLookups = 0

  def staticEvaluation(board : Board) =
    evaluatePositions.get(board) match
      case Some(value) => 
        tableLookups += 1
        value
      case None => 
        val value = evaluate(board)
        evaluatePositions = evaluatePositions + (board -> value)
        value

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




