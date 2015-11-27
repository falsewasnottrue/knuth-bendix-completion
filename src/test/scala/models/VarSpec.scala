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
  }
}
