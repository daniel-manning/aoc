package models

case class Position(x:Int, y:Int) {
  def add(vector: (Int, Int)): Position = {
    Position(this.x + vector._1, this.y + vector._2)
  }
}
