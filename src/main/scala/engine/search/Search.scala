package engine.search

import chess._

trait Search: 
  def search(board : Board, depth : Int) : Move 