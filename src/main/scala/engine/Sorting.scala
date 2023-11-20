package engine

import chess._

object Sorting:

  /* Returns true if move one is more likely to be a good move than move two */
  def compareMoves(move1: Move, move2: Move): Boolean =
    var move1Score = 0
    var move2Score = 0
    if move1.isCapture then move1Score += 100
    if move2.isCapture then move2Score += 100
    
    move1Score > move2Score