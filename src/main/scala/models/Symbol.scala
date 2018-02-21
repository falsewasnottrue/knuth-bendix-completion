package models

trait Symbol {
  def name: String

  def toVerboseString: String
  def toReadableString: String

  override def toString() = toVerboseString
}

case class FunctionSymbol(name: String, arity: Int) extends Symbol {
  override def toVerboseString = s"($name, $arity)"
  override def toReadableString: String = name
}

object FunctionSymbol {
  private val fss = scala.collection.mutable.Map[(String, Int), FunctionSymbol]()

  def of(name: String, arity: Int): FunctionSymbol =
    fss.getOrElseUpdate((name, arity), new FunctionSymbol(name, arity))
}

case class VarSymbol(name: String) extends Symbol {
  override def toVerboseString = s"Var($name)"
  override def toReadableString: String = name
}

object VarSymbol {
  private val vars = scala.collection.mutable.Map[String, VarSymbol]()
  def of(name: String): VarSymbol = vars.getOrElseUpdate(name, new VarSymbol(name))

  implicit def varSymbol2Var(varSymbol: VarSymbol): Term = Var(varSymbol)
}
