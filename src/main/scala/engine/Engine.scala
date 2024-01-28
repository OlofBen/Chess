package engine

import engine.search.AlfaBeta
import engine.search._
import engine.static_eval._
import chess._
import engine.static_eval.simplified_evaluation_function.Simplified_Evaluation_Function


class Engine(search : Search = AlfaBeta):
  
  val searchDepth = search.search
  
  def bestMove(board: Board, depth: Int): Move = 
    val result = searchDepth(board, depth)
    assert(result != null, "No move found")
    result

  def bestMoveAfterTime(board: Board, timeInMillis: Int): Move = //TODO Save the best moves from the previous iteration
    val startTime = System.currentTimeMillis()
    val endTime = startTime + timeInMillis
    var currentDepth = 1
    var bestMove = searchDepth(board, currentDepth)
    while System.currentTimeMillis() < endTime do // TODO Interrupt if time is up
      currentDepth += 1
      bestMove = searchDepth(board, currentDepth)
    bestMove