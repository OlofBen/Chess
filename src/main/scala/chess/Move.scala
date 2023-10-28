package chess 

case class Move(from: Position, to: Position):
  override def toString: String = s"$from$to"

object Move:
  def apply(form : String, to: String): Move =
    Move(Position(form), Position(to))