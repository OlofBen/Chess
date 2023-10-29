import chess._

object Game:
  var board = Board.startingPosition()
  @main
  def run() = 
    while true do 
      println(board)
      val move = scala.io.StdIn.readLine()
      board = board.move(move)
      println(board)
      val legalMoves = board.legalMoves(Color.Black)
      val computerMove : Move = legalMoves.toSeq(scala.util.Random.nextInt(legalMoves.size))
      board = board.move(computerMove)
