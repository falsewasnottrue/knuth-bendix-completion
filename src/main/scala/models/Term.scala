package models

import language.implicitConversions

sealed trait Term

case class Var(varSymbol: VarSymbol) extends Term {
  override def toString = varSymbol.name
}

case class Func(funcSymbol: FuncSymbol, terms: Term*) extends Term {
  assert(terms.length == funcSymbol.arity, "Mismatch of arity and terms")
  override def toString = funcSymbol.name + terms.mkString("(", ",", ")")
}
