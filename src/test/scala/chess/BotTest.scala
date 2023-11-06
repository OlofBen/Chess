package chess

import org.scalatest.funsuite.AnyFunSuite
import chess.pieces._

class BotFunSuite extends AnyFunSuite:
  test("Bot should be able to calculate 4 moves in") {
    val board = Board.startingPosition().move(Move("e2","e4"))
    val bot = Bot()
    val move = bot.bestMove(board, 4)
    require(move != null)
  }

  test("Mate in one") { 
    val pieces = Seq(
      King(Position("a1"), Color.White),
      King(Position("a8"), Color.Black),
      Rook(Position("g6"), Color.White),
      Rook(Position("h7"), Color.White)
    )
    val board = Board.emptyWith(pieces)
    val bestMove = Bot().bestMove(board, 1)
    require(bestMove == Move("g6", "g8"), s"Wrong best move. Bot move $bestMove")
  }