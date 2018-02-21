package models

case class Pos(pos: List[Int]) {
  def +(that: Int): Pos = Pos(pos :+ that)
  override def toString = pos mkString "."
}

object Pos {
  val zero = Pos(List(0))
}
