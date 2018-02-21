package models

import org.specs2.mutable.Specification

class SymbolSpec extends Specification {

  "FunctionSymbol" should {

    "be unique with same name and same arity" in {
      val fs1 = FunctionSymbol.of("f", 2)
      val fs2 = FunctionSymbol.of("f", 2)

      fs1 must equalTo(fs2)
      fs1 must be_===(fs2)
    }

    "be different for different arities" in {
      val fs1 = FunctionSymbol.of("f", 1)
      val fs2 = FunctionSymbol.of("f", 2)

      fs1 must be_!=(fs2)
    }

    "be different for different names" in {
      val fs1 = FunctionSymbol.of("f", 1)
      val fs2 = FunctionSymbol.of("g", 1)

      fs1 must be_!=(fs2)
    }

    "implement toString" in {
      val fs = FunctionSymbol("f", 1)
      fs.toString must equalTo("(f, 1)")
    }
  }

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

      v.toReadableString must equalTo("x")
      v.toVerboseString must equalTo("Var(x)")
    }
  }
}
