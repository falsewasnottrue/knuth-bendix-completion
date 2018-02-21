package models

import org.specs2.mutable.Specification

class TermSpec extends Specification{

  "Term" should {
    "be buildable for a variable" in {
      val x = VarSymbol.of("x")

      val term: Term = x
      term.toString must equalTo("x")
    }

    "be buildable with no argument" in {
      val f = FunctionSymbol.of("f", 0)

      val term: Term = Func(f)
      term.toString must equalTo("f()")
    }

    "be buildable with one argument" in {
      val x = VarSymbol.of("x")
      val f = FunctionSymbol.of("f", 1)

      val term: Term = Func(f, x)
      term.toString must equalTo("f(x)")
    }

    "be buildable with more than one argument" in {
      val x = VarSymbol.of("x")
      val f = FunctionSymbol.of("f", 2)

      val term: Term = Func(f, x, x)
      term.toString must equalTo("f(x,x)")
    }

    "throw Assertion error for wrong arity" in {
      val f = FunctionSymbol.of("f", 1)
      Func(f) must throwAn[AssertionError]

      val x = VarSymbol.of("x")
      Func(f,x,x) must throwAn[AssertionError]
    }
  }
}
