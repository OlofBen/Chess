package chess

import org.scalatest.funsuite.AnyFunSuite

class BotFunSuite extends AnyFunSuite:
  test("Bot should be able to calculate 4 moves in") {
    val board = Board.startingPosition().move(Move("e2","e4"))
    val bot = Bot()
    val move = bot.bestMove(board, 4)
    require(move != null)
  }