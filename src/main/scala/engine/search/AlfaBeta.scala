package engine.search

import chess._
import engine.static_eval.StaticEvaluator
import engine.Sorting

object AlfaBeta extends Search:

  override def search(board: Board, depth: Int): Move = 
    alphaBetaIterative(board, depth, Double.MinValue, Double.MaxValue)._2

  def alphaBetaIterative(board: Board, depth: Int, alpha : Double, beta: Double)(using staticEval : StaticEvaluator): (Double, Move) = //returns best move and its score
    import scala.math.{max, min}
    lazy val isMaximizingPlayer = board.turn == Color.White
    lazy val movesIterator = board.legalMoves.sortWith(Sorting.compareMoves).iterator
    var bestMove: Move = null
    var cutoff = false

    if depth == 0 || board.isGameOver then 
      (staticEval.evaluate(board), bestMove)
    else if isMaximizingPlayer then
      var bestValue = Double.MinValue
      var currentAlpha = alpha
      while movesIterator.hasNext && !cutoff do 
        val move = movesIterator.next
        val nextMove = alphaBetaIterative(board.move(move), depth - 1, currentAlpha, beta)
        if nextMove._1 > bestValue then 
          bestValue = nextMove._1
          bestMove = move
        cutoff = bestValue > beta
        currentAlpha = max(currentAlpha, bestValue)
      (bestValue, bestMove)

    else 
      var bestValue = Double.MaxValue
      var currentBeta = beta
      while movesIterator.hasNext && !cutoff do 
        val move = movesIterator.next
        val nextMove = alphaBetaIterative(board.move(move), depth - 1, alpha, currentBeta)
        if nextMove._1 < bestValue then 
          bestValue = nextMove._1
          bestMove = move
        cutoff = bestValue < alpha
        currentBeta = min(currentBeta, bestValue)
      (bestValue, bestMove)   