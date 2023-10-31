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
      if board.turn == Color.White then 
        legalMoves.collect( move => move match     
          case _ if beta > alpha => //Else we have already found a better move
            val value = evaluate(board.move(move), depth - 1, currentAlpha, currentBeta)
            currentAlpha = Math.max(currentAlpha, value)
            value
        ).maxOption.getOrElse(Int.MinValue)
      else 
        legalMoves.collect( move => move match
          case _ if beta > alpha => 
            val value = evaluate(board.move(move), depth - 1, currentAlpha, currentBeta)
            currentBeta = Math.min(currentBeta, value)
            value
        ).minOption.getOrElse(Int.MaxValue)


  def staticEvaluation(board : Board) = 
    boardsEvaluated += 1
    Evaluation.evaluate(board)