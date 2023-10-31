package chess

class Bot():
  var boardsEvaluated = 0

  def bestMove(board: Board, depth: Int): Move =
    evaluate(board, depth, Int.MinValue, Int.MaxValue)
      ._2
      .getOrElse(throw new Exception("No move found"))

  private def evaluateMove(board: Board, move: Move, depth: Int, alpha : Int, beta : Int): (Int, Move) =
    val newBoard = board.move(move)
    if depth == 0 || board.legalMoves.isEmpty then
      (staticEvaluation(newBoard), move)
    else
      val result = evaluate(newBoard, depth, alpha, beta)
      if result._2.isEmpty then 
        (staticEvaluation(newBoard), move)
      else 
        (result._1, move)
      
      

  private def evaluate(board: Board, depth: Int, alpha : Int, beta : Int): (Int, Option[Move]) =
    val legalMoves = board.legalMoves
    var currentAlpha = alpha
    var currentBeta = beta
    val getBestValue: Set[(Int, Move)] => (Int, Option[Move]) = 
      if board.turn == Color.White then 
        _.maxByOption(_._1).map((v, m) => (v, Some(m))).getOrElse((Int.MinValue, None))
      else 
        _.minByOption(_._1).map((v, m) => (v, Some(m))).getOrElse((Int.MaxValue, None))
    
    val scores = legalMoves.collect( move => move match     
      case _ if beta > alpha => //Else we have already found a better move
        val value = evaluateMove(board, move, depth - 1, currentAlpha, currentBeta)._1
        if board.turn == Color.White then
          currentAlpha = Math.max(currentAlpha, value)
        else
          currentBeta = Math.min(currentBeta, value)
        (value, move)
    ) 
    getBestValue(scores)
     


  def staticEvaluation(board : Board) = 
    boardsEvaluated += 1
    Evaluation.evaluate(board)