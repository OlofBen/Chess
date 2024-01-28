package engine


import org.scalatest.funsuite.AnyFunSuite
import chess.pieces._
import chess._
import engine.search.AlfaBetaV2

class BotFunSuite extends AnyFunSuite:
  test("Bot should be able to calculate 4 moves in V1") {
    val t0 = System.currentTimeMillis()
    val board = Board.startingPosition()
    val bot = Engine()
    val move = bot.bestMove(board, 4)
    require(move != null)
    val t1 = System.currentTimeMillis()
    println(s"V1 time: ${t1 - t0}")
  }

  test("Bot should be able to calculate 4 moves in V2") {
    val t0 = System.currentTimeMillis()
    val board = Board.startingPosition()
    val bot = Engine(search = AlfaBetaV2)
    val move = bot.bestMove(board, 4)
    require(move != null)
    val t1 = System.currentTimeMillis()
    println(s"V2 time: ${t1 - t0}")
  }

  test("Mate in one") { 
    val pieces = Seq(
      King(Position("a1"), Color.White),
      King(Position("a8"), Color.Black),
      Rook(Position("g6"), Color.White),
      Rook(Position("h7"), Color.White)
    )
    val board = Board.emptyWith(pieces)
    val bestMove = Engine().bestMove(board, 1)
    require(bestMove == Move("g6", "g8"), s"Wrong best move. Bot move $bestMove")
  }

  test("Jonathan") {
    val board = Board.fromFen("8/8/2p1k3/2Bn4/8/8/P5PP/7K w - - 0 46")
    val bestMove = Engine().bestMove(board, 4)
    println(bestMove)
  }

  test("Morning game") {
    val board = Board.fromFen("r2q1r2/pQp3k1/3pbnpp/4pn2/5p2/2NP1N2/PPP1PPPP/2KR1B1R w - - 4 18")
    val bestMove = Engine().bestMove(board, 4)
    println(bestMove)
  }