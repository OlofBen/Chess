package chess 

case class Move(
    from: Position, 
    to: Position, 
    isCapture: Boolean = false, 
    isPawnMovingTwo: Boolean = false,
    isEnPassantCapture: Boolean = false,
    promotionPiece: Option[Piece] = None,
    isCastle: Boolean = false
  ):
  override def toString: String = s"$from$to"
  override def hashCode(): Int = (from, to).hashCode()
  override def equals(other: Any): Boolean = 
    other match
      case other: Move => from == other.from && to == other.to
      case _ => false

object Move:
  def apply(form : String, to: String): Move =
    Move(Position(form), Position(to))