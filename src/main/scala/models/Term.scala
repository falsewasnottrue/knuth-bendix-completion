package models

import language.implicitConversions

sealed trait Term {
  type Substitution = Map[Var, Term]

  def positions: List[Pos]
  def terms: List[Term]

  def rewrite(s: Substitution): Term
}

case class Func(funcSymbol: FunctionSymbol, ts: Term*) extends Term {
  assert(ts.length == funcSymbol.arity, "Mismatch of arity and terms")

  override def positions: List[Pos] = positionsFrom(Pos.zero)

  private def positionsFrom(prefix: Pos): List[Pos] =
    prefix :: ts.toList.zipWithIndex.flatMap {
      case (term, index) => term match {
        case Var(_) => List(prefix + index)
        case f: Func => f.positionsFrom(prefix + index)
      }
    }

  override def terms: List[Term] = this :: ts.toList.flatMap(_.terms)

  override def rewrite(s: Substitution): Term = Func(funcSymbol, ts.map(_.rewrite(s)):_*)

  override def toString = funcSymbol.name + ts.mkString("(", ",", ")")
}


case class Var(varSymbol: VarSymbol) extends Term {
  override val positions: List[Pos] = List(Pos.zero)
  override val terms: List[Term] = List(this)

  override def rewrite(s: Substitution): Term = s.getOrElse(this, this)

  override def toString = varSymbol.name
}

object Var {
  def of(name: String) = new Var(VarSymbol.of(name))
}

