package models

case class Var(name: String) extends Term {
  override def toString = name
}

object Var {
  private val vars = scala.collection.mutable.Map[String, Var]()
  def of(name: String): Var = vars.getOrElseUpdate(name, new Var(name))
}
