package models

import org.specs2.mutable.Specification

class FuncSpec extends Specification {

  "Func" should {
    "implement positions for no argument" in {
      val fs = FuncSymbol("f", 0)
      val t: Term = Func(fs)
      val ps = t.positions

      ps.length must equalTo(1)
    }

    "implement positions for single argument" in {
      val fs = FuncSymbol("f", 1)
      val t: Term = Func(fs, Var.of("x"))
      // f(x) -> (0, 0.0)
      val ps = t.positions

      ps.length must equalTo(2)
      ps must equalTo(Position(List(0)) :: Position(List(0,0)) :: Nil)
    }

    "implement positions for two arguments" in {
      val fs = FuncSymbol("f", 2)
      val x = Var.of("x")
      // f(x,x) -> (0, 0.0, 0.1)
      val t: Term = Func(fs, x, x)

      val ps = t.positions

      ps.length must equalTo(3)
      ps must equalTo(Position(List(0)) :: Position(List(0,0)) :: Position(List(0,1)) :: Nil)
    }

    "implement positions for two arguments" in {
      val f = FuncSymbol("f", 2)
      val x = Var.of("x")
      // f(f(x,x),x) -> (0, 0.0, 0.1, 0.0.0, 0.0.1)
      val t: Term = Func(f, Func(f, x, x), x)

      val ps = t.positions

      ps.length must equalTo(5)
      ps must equalTo(Position(List(0)) :: Position(List(0,0))
        :: Position(List(0,0,0)) :: Position(List(0,0,1))
        :: Position(List(0,1)) :: Nil)
    }

    "implement terms for no argument" in {
      val fs = FuncSymbol("f", 0)
      val t: Term = Func(fs)
      val ts = t.terms

      ts.length must equalTo(1)
      ts.head must equalTo(t)
    }

    "implement terms for single argument" in {
      val f = FuncSymbol("f", 1)
      val x = Var.of("x")
      val t: Term = Func(f, x)
      // f(x) -> (0, 0.0)
      val ts = t.terms

      ts.length must equalTo(2)
      ts must equalTo(t :: x :: Nil)
    }

    "implement terms for two arguments" in {
      val f = FuncSymbol("f", 2)
      val x = Var.of("x")
      // f(x,x) -> (0, 0.0, 0.1)
      val t: Term = Func(f, x, x)

      val ts = t.terms

      ts.length must equalTo(3)
      ts must equalTo(t :: x :: x :: Nil)
    }

    "implement terms for two arguments" in {
      val f = FuncSymbol("f", 2)
      val x = Var.of("x")
      // f(f(x,x),x) -> (0, 0.0, 0.1, 0.0.0, 0.0.1)
      val t: Term = Func(f, Func(f, x, x), x)

      val ts = t.terms

      ts.length must equalTo(5)
      ts must equalTo(t :: Func(f, x, x) :: x :: x :: x :: Nil)
    }
  }
}