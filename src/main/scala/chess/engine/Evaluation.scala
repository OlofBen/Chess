package chess.engine

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

    
  
  def score(piece : Piece):Int = 
    piece match
      case _: King => 1000000
      case _: Queen => 9
      case _: Rook => 5
      case _: Bishop => 3
      case _: Knight => 3
      case _: Pawn => 1


