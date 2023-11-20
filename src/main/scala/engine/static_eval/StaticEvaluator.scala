package engine.static_eval

import chess._

trait StaticEvaluator:
  def evaluate(board: Board): Int

