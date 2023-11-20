package engine.search

import chess._
import engine.static_eval.StaticEvaluator

object SearchAll extends Search:

  override def search(staticEval: StaticEvaluator)(board: Board, depth: Int): Move = 
    val bestMoveOption = 
      if board.turn == Color.White then 
        board.legalMoves.maxByOption( move => 
          val newBoard = board.move(move)
          recursiveSearch(newBoard, depth - 1, staticEval)
        )
      else 
        board.legalMoves.minByOption( move => 
          val newBoard = board.move(move)
          recursiveSearch(newBoard, depth - 1, staticEval)
        )
    bestMoveOption.getOrElse(throw new Exception("No move found"))

  def recursiveSearch(board: Board, depth: Int, staticEval:StaticEvaluator): Int = 
    if depth == 0 || board.isGameOver then 
      staticEval.evaluate(board)
    else 
      val scores = board.legalMoves.map( move => 
        val newBoard = board.move(move)
        recursiveSearch(newBoard, depth - 1, staticEval)
      )
      if board.turn == Color.White then 
        scores.max
      else 
        scores.min

 
