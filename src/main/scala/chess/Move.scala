package chess 

case class Move(from: Position, to: Position):
  override def toString: String = s"$from$to"