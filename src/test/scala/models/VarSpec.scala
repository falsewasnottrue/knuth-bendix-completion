package models

import org.specs2.mutable.Specification

class VarSpec extends Specification {

  "Var" should {
    "have exactly one position" in {
      val v = Var.of("x")

      v.positions.length must equalTo(1)
    }

    "have exactly one term" in {
      val v = Var.of("x")

      v.terms.length must equalTo(1)
      v.terms.head must equalTo(v)
    }

    "rewrite with empty substitution" in {
      val v = Var.of("x")
      val subst = Map[Var, Term]()

      v.rewrite(subst) must equalTo(v)
    }

    "rewrite with disjoint substitution" in {
      val v = Var.of("x")
      val subst = Map[Var, Term](Var.of("y") -> v)

      v.rewrite(subst) must equalTo(v)
    }

    "rewrite with actual substitution" in {
      val v = Var.of("x")
      val subst = Map[Var, Term](v -> Var.of("y"))

      v.rewrite(subst) must equalTo(Var.of("y"))
    }
  }
}
