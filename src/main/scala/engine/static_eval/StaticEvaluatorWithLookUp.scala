package engine.static_eval

import chess._
import util.MaxSizeMap

class StaticEvaluatorWithLookUp(eval : StaticEvaluator) extends StaticEvaluator:
  private var cache : Map[Board, Double] = MaxSizeMap.empty()
  def evaluate(board: Board): Double =
    if cache.contains(board) then
      cache(board)
    else
      val score = eval.evaluate(board)
      cache = cache + (board -> score)
      score
