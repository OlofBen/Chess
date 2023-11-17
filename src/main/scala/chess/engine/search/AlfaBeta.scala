package chess.engine.search

import chess._
import chess.engine.static_eval.StaticEvaluator


object AlfaBeta extends Search:


  override def search(staticEval: StaticEvaluator)(board: Board, depth: Int): Move = 
    alphaBetaIterative(board, depth, Int.MinValue, Int.MaxValue, staticEval)._2

  def alphaBetaIterative(board: Board, depth: Int, alpha : Int, beta: Int, staticEval : StaticEvaluator): (Int, Move) = //returns best move and its score
    import scala.math.{max, min}
    lazy val isMaximizingPlayer = board.turn == Color.White
    lazy val movesIterator = board.legalMoves.iterator
    var bestMove: Move = null
    var cutoff = false

    if depth == 0 || board.isGameOver then 

      (staticEval.evaluate(board), bestMove)
    else if isMaximizingPlayer then
      var bestValue = Int.MinValue
      var currentAlpha = alpha
      while movesIterator.hasNext && !cutoff do 
        val move = movesIterator.next
        val nextMove = alphaBetaIterative(board.move(move), depth - 1, currentAlpha, beta, staticEval)
        if nextMove._1 > bestValue then 
          bestValue = nextMove._1
          bestMove = move
        cutoff = bestValue > beta
        currentAlpha = max(currentAlpha, bestValue)
      (bestValue, bestMove)

    else 
      var bestValue = Int.MaxValue
      var currentBeta = beta
      while movesIterator.hasNext && !cutoff do 
        val move = movesIterator.next
        val nextMove = alphaBetaIterative(board.move(move), depth - 1, alpha, currentBeta, staticEval)
        if nextMove._1 < bestValue then 
          bestValue = nextMove._1
          bestMove = move
        cutoff = bestValue < alpha
        currentBeta = min(currentBeta, bestValue)
      (bestValue, bestMove)   