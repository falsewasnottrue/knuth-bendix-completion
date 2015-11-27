package models

case class FuncSymbol(name: String, arity: Int) {
  override def toString = s"($name, $arity)"
}

object FuncSymbol {
  private val fss = scala.collection.mutable.Map[(String, Int), FuncSymbol]()

  def of(name: String, arity: Int): FuncSymbol =
    fss.getOrElseUpdate((name, arity), new FuncSymbol(name, arity))
}
