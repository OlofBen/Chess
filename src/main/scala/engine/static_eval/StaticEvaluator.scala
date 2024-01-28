package engine.static_eval

import chess._
import engine.static_eval.simplified_evaluation_function.Simplified_Evaluation_Function

trait StaticEvaluator:
  def evaluate(board: Board): Int

object StaticEvaluator: 
  given StaticEvaluator = Simplified_Evaluation_Function

