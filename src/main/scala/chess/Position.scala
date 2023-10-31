package chess

case class Position(row: Int, col: Int):
  def moved(rowDelta: Int = 0, colDelta: Int = 0): Position = 
    Position(row + rowDelta, col + colDelta)

  /**
    * @return the distance between two positions, a diagonal move counts as 1
    */
  def distanceTo(other : Position) : Int = 
    val rowDelta = (row - other.row).abs
    val colDelta = (col - other.col).abs
    Math.max(rowDelta, colDelta)
  
  override def toString(): String = 
    s"${Position.rowToLetter.getOrElse(col, "?")}$row"

  lazy val isInside = 
    row >= 1 && row <= 8 && col >= 1 && col <= 8



object Position:
  val rowToLetter = Map(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d", 5 -> "e", 6 -> "f", 7 -> "g", 8 -> "h")

  def apply(pos: String): Position = 
    val col = pos(0).toLower - 'a' + 1
    val row = pos(1).asDigit
    new Position(row, col)