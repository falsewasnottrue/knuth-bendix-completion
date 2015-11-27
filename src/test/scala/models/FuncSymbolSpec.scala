package models

import org.specs2.mutable.Specification

class FuncSymbolSpec extends Specification {

  "FuncSymbol" should {
    "be unique with same name and same arity" in {
      val fs1 = FuncSymbol.of("f", 2)
      val fs2 = FuncSymbol.of("f", 2)

      fs1 must equalTo(fs2)
      fs1 must be_===(fs2)
    }

    "be different for different arities" in {
      val fs1 = FuncSymbol.of("f", 1)
      val fs2 = FuncSymbol.of("f", 2)

      fs1 must be_!=(fs2)
    }

    "be different for different names" in {
      val fs1 = FuncSymbol.of("f", 1)
      val fs2 = FuncSymbol.of("g", 1)

      fs1 must be_!=(fs2)
    }

    "implement toString" in {
      val fs = FuncSymbol("f", 1)
      fs.toString must equalTo("(f, 1)")
    }
  }
}
