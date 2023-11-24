package engine.search

import chess._
import engine.static_eval.StaticEvaluator
import scala.annotation.static

object SearchAll extends Search:
  var isRecording = false // Used for testing
  var movesPreformed = Vector.empty[Move] // Used for testing

  override def search(staticEval: StaticEvaluator)(board: Board, depth: Int): Move = 
    val bestMoveOption = 
      val allScoresWithMoves = scores(board, depth, staticEval).zip(board.legalMoves)
      if board.turn == Color.White then 
        allScoresWithMoves.maxByOption(_._1).map(_._2)
      else 
        allScoresWithMoves.minByOption(_._1).map(_._2)
        
    bestMoveOption.getOrElse(throw new Exception("No move found"))

  def recursiveSearch(board: Board, depth: Int, staticEval:StaticEvaluator): Int = 
    if depth == 0 || board.isGameOver then 
      staticEval.evaluate(board)
    else 
      val scoresAll = scores(board, depth, staticEval)
      if board.turn == Color.White then 
        scoresAll.max
      else 
        scoresAll.min

  private def scores(board: Board, depth: Int, staticEval: StaticEvaluator): Iterable[Int] = 
    if isRecording then 
      movesPreformed ++= board.legalMoves
    board.legalMoves.map( move => 
      val newBoard = board.move(move)
      recursiveSearch(newBoard, depth - 1, staticEval)
    )
