package chess

class Bot():
  var boardsEvaluated = 0

  def bestMove(board: Board, depth: Int): Move =
    val legalMoves = board.legalMoves
    legalMoves.maxBy(move => evaluate(board.move(move), depth - 1, Int.MinValue, Int.MaxValue))

  private def evaluate(board: Board, depth: Int, alpha : Int, beta : Int): Int =
    val legalMoves = board.legalMoves
    if depth == 0 || legalMoves.isEmpty then 
      staticEvaluation(board)
    else
      var currentAlpha = alpha
      var currentBeta = beta
      val getBestValue: Set[Int] => Int = if board.turn == Color.White then _.maxOption.getOrElse(Int.MinValue) else _.minOption.getOrElse(Int.MaxValue)
      
      val scores = legalMoves.collect( move => move match     
        case _ if beta > alpha => //Else we have already found a better move
          val value = evaluate(board.move(move), depth - 1, currentAlpha, currentBeta)
          if board.turn == Color.White then
            currentAlpha = Math.max(currentAlpha, value)
          else
            currentBeta = Math.min(currentBeta, value)
          value
      ) 
      getBestValue(scores)
     


  def staticEvaluation(board : Board) = 
    boardsEvaluated += 1
    Evaluation.evaluate(board)