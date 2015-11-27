package models

import org.specs2.mutable.Specification

class TermSpec extends Specification{

  "Term" should {
    "be builadble for a variable" in {
      val x = VarSymbol.of("x")

      val term: Term = x
      term.toString must equalTo("x")
    }

    "be builadble with no argument" in {
      val f = FuncSymbol.of("f", 0)

      val term: Term = Func(f)
      term.toString must equalTo("f()")
    }

    "be builadble with one argument" in {
      val x = VarSymbol.of("x")
      val f = FuncSymbol.of("f", 1)

      val term: Term = Func(f, x)
      term.toString must equalTo("f(x)")
    }

    "be builadble with more than one argument" in {
      val x = VarSymbol.of("x")
      val f = FuncSymbol.of("f", 2)

      val term: Term = Func(f, x, x)
      term.toString must equalTo("f(x,x)")
    }

    "throw Assertion error for wrong arity" in {
      val f = FuncSymbol.of("f", 1)
      Func(f) must throwAn[AssertionError]

      val x = VarSymbol.of("x")
      Func(f,x,x) must throwAn[AssertionError]
    }
  }
}
