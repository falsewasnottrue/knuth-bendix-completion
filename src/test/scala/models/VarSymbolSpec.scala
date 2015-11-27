package models

import org.specs2.mutable.Specification

class VarSymbolSpec extends Specification {

  "VarSymbols" should {
    "be unique for the same name" in {
      val v1 = VarSymbol.of("x")
      val v2 = VarSymbol.of("x")

      v1 must equalTo(v2)
      v1 must be_===(v2)
    }

    "be different for different names" in {
      VarSymbol.of("x") must be_!==(VarSymbol.of("y"))
    }

    "implement toString" in {
      val v = VarSymbol.of("x")
      v.name must equalTo(v.toString)
    }
  }
}
