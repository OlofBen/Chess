package engine.static_eval

import chess.Board

class EvalCounter(eval : StaticEvaluator) extends StaticEvaluator:
  private var counter = 0
  override def evaluate(board: Board): Int = 
    counter += 1
    eval.evaluate(board)

  def getCounter() = counter

  def resetCounter() = counter = 0

