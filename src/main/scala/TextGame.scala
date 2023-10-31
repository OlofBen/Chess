import chess._

object Game:
  var board = Board.startingPosition()
  println(board)
  @main
  def run() = 
    while true do 
      val t0 = System.currentTimeMillis()
      val whiteBot = Bot()
      val whiteMove : Move = whiteBot.bestMove(board, 4)
      println(s"Time: ${System.currentTimeMillis() - t0}ms")
      println(s"Boards evaluated: ${whiteBot.boardsEvaluated}")
      board = board.move(whiteMove)
      println(board)
      val t1 = System.currentTimeMillis()
      val blackBot = Bot()
      val blackMove : Move = blackBot.bestMove(board, 4)
      println(s"Time: ${System.currentTimeMillis() - t1}ms")
      println(s"Boards evaluated: ${blackBot.boardsEvaluated}")
      board = board.move(blackMove)
      println(board)
