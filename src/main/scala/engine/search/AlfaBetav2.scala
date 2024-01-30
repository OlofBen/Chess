package engine.search

import chess.Move
import chess.Board
import chess.Color
import engine.static_eval.StaticEvaluator
import scala.math.{max, min}
import util.MaxSizeMap

object AlfaBetaV2 extends Search: 

  override def search(board: Board, depth: Int): Move = 
    var currentBestMove = board.legalMoves.head
    for currentSearch <- 1 to depth do  
      currentBestMove = bestMove(board, currentSearch, Double.MinValue, Double.MaxValue)._1
    currentBestMove


  case class BoardSearch(depth: Int, sortedMoves : Iterable[Move])

  var sorted : Map[Board, BoardSearch] = MaxSizeMap.empty()

  def bestMove(board: Board, depth: Int, alfa : Double, beta : Double): (Move, Double) = 
    //What if there are no leagal moves?
    if ! sorted.contains(board) then search(board, depth, alfa, beta)  
    else 
      val boardSearch = sorted(board)
      searchWithOrder(board, depth, boardSearch.sortedMoves, alfa, beta)

  def search(board: Board, depth: Int, alfa : Double, beta : Double): (Move, Double) = 
    val orderedMoves = board.legalMoves //And sort them
    searchWithOrder(board, depth, orderedMoves, alfa, beta) 

  def searchWithOrder(board: Board, depth: Int, sortedMoves : Iterable[Move], alfa : Double, beta : Double): (Move, Double) = 
    if sortedMoves.size == 0 then (null, evaluateStatic(board))
    else if depth == 1 then 
      depthOne(board, sortedMoves, alfa, beta)
    else 
      evaluate(board, sortedMoves, alfa, beta, (nextBoard, a, b) => bestMove(nextBoard, depth - 1, a, b)._2)


  def evaluate(
    board : Board, 
    moves : Iterable[Move],
    alfa0 : Double, 
    beta0 : Double, 
    nextDepth : ((Board, Double, Double) => Double),
  ) = 
    val isMaximizingPlayer = board.turn == Color.White 
    var alfa = alfa0
    var beta = beta0
    var bestScore = if isMaximizingPlayer then Double.MinValue else Double.MaxValue
    var bestMove : Move = moves.head
    var shouldBreak = false
    val movesIterator : Iterator[Move]= moves.iterator
    var evaluatedMoves = Vector.empty[(Move, Double)]
    while movesIterator.hasNext && !shouldBreak do
      val move = movesIterator.next()
      val newBoard = board.move(move)
      val score = nextDepth(newBoard, alfa, beta)
      evaluatedMoves = evaluatedMoves :+ (move, score)
      if isMaximizingPlayer then 
        if score > bestScore then 
          bestScore = score
          bestMove = move
          shouldBreak = score > beta
        alfa = max(alfa, score)
      else
        if score < bestScore then 
          bestScore = score
          bestMove = move
          shouldBreak = score < alfa
        beta = min(beta, score)

    val sortedMoves = if isMaximizingPlayer then evaluatedMoves.sortBy(_._2).reverse else evaluatedMoves.sortBy(_._2)
    val totalMoves : Iterable[Move] = 
      sortedMoves.map(_._1) 
      ++ movesIterator.toVector 
    val boardSearch = BoardSearch(1, totalMoves)
    sorted = sorted + (board -> boardSearch)
    (bestMove, bestScore)

  def depthOne(board: Board, moves : Iterable[Move], alfa: Double, beta: Double)(using staticEval: StaticEvaluator): (Move, Double) = 
    val finalDepth: (Board, Double, Double) => Double = (board, _, _) => evaluateStatic(board)
    evaluate(board, moves, alfa, beta, finalDepth)

  def evaluateStatic(board: Board)(using staticEval: StaticEvaluator) = staticEval.evaluate(board)
