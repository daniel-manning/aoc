package models

import Year2019.TwoVector

case class Position(x:Int, y:Int) {
  def +(vector: TwoVector): Position =
    Position(this.x + vector.x, this.y + vector.y)
}
