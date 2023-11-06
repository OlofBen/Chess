import chess.engine._
import chess._

object Game:
  var board = Board.startingPosition()
  println(board)
  val whiteBot = Engine()
  val blackBot = Engine()
  @main
  def run() = 
    while true do 
      val t0 = System.currentTimeMillis()
      
      val whiteMove : Move = whiteBot.bestMove(board, 4)
      println(s"Time: ${System.currentTimeMillis() - t0}ms")
      board = board.move(whiteMove)
      println(board)
      val t1 = System.currentTimeMillis()
      
      val blackMove : Move = blackBot.bestMove(board, 4)
      println(s"Time: ${System.currentTimeMillis() - t1}ms")
      board = board.move(blackMove)
      println(board)
