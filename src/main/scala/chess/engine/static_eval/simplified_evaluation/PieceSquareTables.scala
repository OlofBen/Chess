package chess.engine.static_eval.simplified_evaluation_function

import chess._
import chess.pieces._
/**
  * Everything here is taken form https://www.chessprogramming.org/Simplified_Evaluation_Function
  */
sealed trait PieceSquareTable:
  protected def get(row: Int, col: Int):Int //Index of table is 0,0 at top left corner aka "a8"
  def apply(piece : Piece): Int = 
    val position = piece.position
    piece.color match
      case Color.White => get(position.row - 1, position.col - 1)
      case Color.Black => get(8 - position.row, position.col - 1)

object PieceSquareTable:
  def apply(piece : Piece) : Int = 
    piece match
      case _: King => KingSquareTable(piece)
      case _: Queen => QueenSquareTable(piece)
      case _: Rook => RookSquareTable(piece)
      case _: Bishop => BishopSquareTable(piece)
      case _: Knight => KnightSquareTable(piece)
      case _: Pawn => PawnSquareTable(piece)

object PawnSquareTable extends PieceSquareTable:
  private val pawn = Array(
    Array(0,  0,  0,  0,  0,  0,  0,  0),
    Array(50, 50, 50, 50, 50, 50, 50, 50),
    Array(10, 10, 20, 30, 30, 20, 10, 10),
    Array(5,  5, 10, 25, 25, 10,  5,  5),
    Array(0,  0,  0, 20, 20,  0,  0,  0),
    Array(5, -5,-10,  0,  0,-10, -5,  5),
    Array(5, 10, 10,-20,-20, 10, 10,  5),
    Array(0,  0,  0,  0,  0,  0,  0,  0),
  )
  protected def get(row: Int, col: Int): Int = pawn(row)(col)
  

object KnightSquareTable extends PieceSquareTable:
  private val knight = Array(
    Array(-50,-40,-30,-30,-30,-30,-40,-50),
    Array(-40,-20,  0,  0,  0,  0,-20,-40),
    Array(-30,  0, 10, 15, 15, 10,  0,-30),
    Array(-30,  5, 15, 20, 20, 15,  5,-30),
    Array(-30,  0, 15, 20, 20, 15,  0,-30),
    Array(-30,  5, 10, 15, 15, 10,  5,-30),
    Array(-40,-20,  0,  5,  5,  0,-20,-40),
    Array(-50,-40,-30,-30,-30,-30,-40,-50),
  )
  protected def get(row: Int, col: Int): Int = knight(row)(col)

object BishopSquareTable extends PieceSquareTable:
  private val bishop = Array(
    Array(-20,-10,-10,-10,-10,-10,-10,-20),
    Array(-10,  0,  0,  0,  0,  0,  0,-10),
    Array(-10,  0,  5, 10, 10,  5,  0,-10),
    Array(-10,  5,  5, 10, 10,  5,  5,-10),
    Array(-10,  0, 10, 10, 10, 10,  0,-10),
    Array(-10, 10, 10, 10, 10, 10, 10,-10),
    Array(-10,  5,  0,  0,  0,  0,  5,-10),
    Array(-20,-10,-10,-10,-10,-10,-10,-20),
  )
  protected def get(row: Int, col: Int): Int = bishop(row)(col)

object RookSquareTable extends PieceSquareTable:
  private val rook = Array(
    Array(0,  0,  0,  0,  0,  0,  0,  0),
    Array(5, 10, 10, 10, 10, 10, 10,  5),
    Array(-5,  0,  0,  0,  0,  0,  0, -5),
    Array(-5,  0,  0,  0,  0,  0,  0, -5),
    Array(-5,  0,  0,  0,  0,  0,  0, -5),
    Array(-5,  0,  0,  0,  0,  0,  0, -5),
    Array(-5,  0,  0,  0,  0,  0,  0, -5),
    Array(0,  0,  0,  5,  5,  0,  0,  0),
  )
  protected def get(row: Int, col: Int): Int = rook(row)(col)

object QueenSquareTable extends PieceSquareTable:
  private val queen = Array(
    Array(-20,-10,-10, -5, -5,-10,-10,-20),
    Array(-10,  0,  0,  0,  0,  0,  0,-10),
    Array(-10,  0,  5,  5,  5,  5,  0,-10),
    Array( -5,  0,  5,  5,  5,  5,  0, -5),
    Array(  0,  0,  5,  5,  5,  5,  0, -5),
    Array(-10,  5,  5,  5,  5,  5,  0,-10),
    Array(-10,  0,  5,  0,  0,  0,  0,-10),
    Array(-20,-10,-10, -5, -5,-10,-10,-20)
  )
  protected def get(row: Int, col: Int): Int = queen(row)(col)

object KingSquareTable extends PieceSquareTable:// Only for eraly - midle game
  private val king = Array(
    Array(-30,-40,-40,-50,-50,-40,-40,-30),
    Array(-30,-40,-40,-50,-50,-40,-40,-30),
    Array(-30,-40,-40,-50,-50,-40,-40,-30),
    Array(-30,-40,-40,-50,-50,-40,-40,-30),
    Array(-20,-30,-30,-40,-40,-30,-30,-20),
    Array(-10,-20,-20,-20,-20,-20,-20,-10),
    Array( 20, 20,  0,  0,  0,  0, 20, 20),
    Array( 20, 30, 10,  0,  0, 10, 30, 20)
  )
  protected def get(row: Int, col: Int): Int = king(row)(col)