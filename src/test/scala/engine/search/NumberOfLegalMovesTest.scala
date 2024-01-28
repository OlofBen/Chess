package engine.search

import chess._
import org.scalatest.funsuite.AnyFunSuite
import engine.static_eval.EvalCounter
import engine.static_eval.MockEval
import engine.static_eval.StaticEvaluator

class NumberOfLegalMovesTest extends AnyFunSuite:

  test("Number of positions reached from stating position") {
    val counter = new EvalCounter(MockEval)
    given StaticEvaluator = counter
    val search = SearchAll.search
    val startingBoard = Board.startingPosition()
    search(startingBoard, 1)
    assert(counter.getCounter() == 20, "Should be 20 moves, but was " + counter.getCounter())
    counter.resetCounter()

    search(startingBoard, 2)
    assert(counter.getCounter() == 400, "Should be 400 moves, but was " + counter.getCounter())
    counter.resetCounter()

    search(startingBoard, 3)
    assert(counter.getCounter() == 8902, "Should be 8902 moves, but was " + counter.getCounter())
    counter.resetCounter()
    
    search(startingBoard, 4)
    assert(counter.getCounter() == 197281, "Should be 197281 moves, but was " + counter.getCounter())
    
  }

  test("Position 2"){
    val counter = new EvalCounter(MockEval)
    given StaticEvaluator = counter
    val search = SearchAll.search

    val board = Board.fromFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ")

    search(board, 1)
    assert(counter.getCounter() == 48, "Should be 48 moves, but was " + counter.getCounter())
    counter.resetCounter()

    
    search(board, 2)
    assert(counter.getCounter() == 2039, "Should be 2039 moves, but was " + counter.getCounter())
    counter.resetCounter()

    
    search(board, 3)
    assert(counter.getCounter() == 97862, "Should be 97862 moves, but was " + counter.getCounter())
    counter.resetCounter()

    //One move of!?!?!?!
    //search(board, 4)
    //assert(counter.getCounter() == 4085603, "Should be 4085603 moves, but was " + counter.getCounter()) // 5873 kort
    //counter.resetCounter()


  }

  test("Position 3"){
    val counter = new EvalCounter(MockEval)
    given StaticEvaluator = counter
    val search = SearchAll.search

    val board = Board.fromFen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")

    search(board, 1)
    assert(counter.getCounter() == 14, "Should be 14 moves, but was " + counter.getCounter())
    counter.resetCounter()

    
    search(board, 2)
    assert(counter.getCounter() == 191, "Should be 191 moves, but was " + counter.getCounter())
    counter.resetCounter()

    
    search(board, 3)
    assert(counter.getCounter() == 2812, "Should be 2812 moves, but was " + counter.getCounter())
    counter.resetCounter()

    search(board, 4)
    assert(counter.getCounter() == 43238, "Should be 43238 moves, but was " + counter.getCounter())
    counter.resetCounter()
  }

  test("Position 4"){
    val counter = new EvalCounter(MockEval)
    given StaticEvaluator = counter
    val search = SearchAll.search

    val board = Board.fromFen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")

    search(board, 1)
    assert(counter.getCounter() == 6, "Should be 6 moves, but was " + counter.getCounter())
    counter.resetCounter()

    
    search(board, 2)
    assert(counter.getCounter() == 264, "Should be 264 moves, but was " + counter.getCounter())
    counter.resetCounter()

    
    search(board, 3)
    assert(counter.getCounter() == 9467, "Should be 9467 moves, but was " + counter.getCounter())
    counter.resetCounter()

    //22 moves of!?!?!?!
    //search(board, 4)
    //assert(counter.getCounter() == 422333, "Should be 422333 moves, but was " + counter.getCounter())
    //counter.resetCounter()
  }

  test("Position 5"){
    val counter = new EvalCounter(MockEval)
    given StaticEvaluator = counter
    
    val search = SearchAll.search
    val startingBoard = Board.fromFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
    search(startingBoard, 1)
    assert(counter.getCounter() == 44, "Should be 44 moves, but was " + counter.getCounter())
    counter.resetCounter()

    search(startingBoard, 2)
    assert(counter.getCounter() ==  1486 , "Should be  1486  moves, but was " + counter.getCounter())
    counter.resetCounter()

    search(startingBoard, 3)
    assert(counter.getCounter() ==  62379 , "Should be  62379  moves, but was " + counter.getCounter())
    counter.resetCounter()
    
    search(startingBoard, 4)
    assert(counter.getCounter() ==  2103487 , "Should be  2103487  moves, but was " + counter.getCounter())
    
  }