package chess.engine

import chess._
import chess.engine.static_eval._
import chess.engine.static_eval.simplified_evaluation_function.Simplified_Evaluation_Function
import chess.engine.search.Search
import chess.engine.search.AlfaBeta
import chess.engine.search.SearchAll

class Engine(search : Search = AlfaBeta , eval: StaticEvaluator = new StaticEvaluatorWithLookUp(Simplified_Evaluation_Function)):
  
  val searchDepth = search.search(eval)
  
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