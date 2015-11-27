package models

case class VarSymbol(name: String) {
  override def toString = name
}

object VarSymbol {
  private val vars = scala.collection.mutable.Map[String, VarSymbol]()
  def of(name: String): VarSymbol = vars.getOrElseUpdate(name, new VarSymbol(name))
}
