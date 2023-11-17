package chess.engine.search

import chess._
import chess.engine.static_eval.StaticEvaluator

trait Search: 
  def search(staticEval : StaticEvaluator)(board : Board, depth : Int) : Move 