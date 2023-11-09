package chess.engine.static_eval

import chess._

class StaticEvaluatorWithLookUp(eval : StaticEvaluator) extends StaticEvaluator:
  private val cache = scala.collection.mutable.Map.empty[Board, Int]
  def evaluate(board: Board): Int =
    if cache.contains(board) then
      cache(board)
    else
      val score = eval.evaluate(board)
      cache += (board -> score)
      score
