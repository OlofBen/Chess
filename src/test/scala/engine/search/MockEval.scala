package engine.static_eval

import chess._

object MockEval extends StaticEvaluator:
  override def evaluate(board: Board): Int = 0 