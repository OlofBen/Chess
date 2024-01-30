package engine.static_eval

import chess._

object MockEval extends StaticEvaluator:
  override def evaluate(board: Board): Double = 0 