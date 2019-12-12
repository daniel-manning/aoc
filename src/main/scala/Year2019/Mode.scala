package Year2019

sealed trait Mode
case object PositionMode extends Mode
case object ImmediateMode extends Mode
case object RelativeMode extends Mode

object Mode {
  def apply(string: Char): Mode = string match {
    case '0' => PositionMode
    case '1' => ImmediateMode
    case '2' => RelativeMode
  }
}
