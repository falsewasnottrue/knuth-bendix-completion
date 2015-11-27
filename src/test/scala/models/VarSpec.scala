package models

import org.specs2.mutable.Specification

class VarSpec extends Specification {

  "Variables" should {
    "be unique for the same name" in {
      val v1 = Var.of("x")
      val v2 = Var.of("x")

      v1 must equalTo(v2)
      v1 must be_===(v2)
    }

    "be different for different names" in {
      Var.of("x") must be_!==(Var.of("y"))
    }

    "implement toString" in {
      val v = Var.of("x")
      v.name must equalTo(v.toString)
    }
  }
}
