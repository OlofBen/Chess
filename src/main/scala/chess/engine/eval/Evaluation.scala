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
      val whitePieces = board.pieces.filter(_.color == Color.White)
      val blackPieces = board.pieces.filter(_.color == Color.Black)
      val whiteScore = whitePieces.toSeq.map(score).sum
      val blackScore = blackPieces.toSeq.map(score).sum
      whiteScore - blackScore

    
  
  def score(piece : Piece):Int = //https://www.chessprogramming.org/Simplified_Evaluation_Function
    piece match
      case _: King => 20000
      case _: Queen => 900
      case _: Rook => 500
      case _: Bishop => 330
      case _: Knight => 320
      case _: Pawn => 100




