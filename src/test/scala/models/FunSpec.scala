package models

import org.specs2.mutable.Specification

class FunSpec extends Specification {

  "Func" should {

    "implement positions for no argument" in {
      val fs = FunctionSymbol("f", 0)
      val t: Term = Func(fs)
      val ps = t.positions

      ps.length must equalTo(1)
    }

    "implement positions for single argument" in {
      val fs = FunctionSymbol("f", 1)
      val t: Term = Func(fs, Var.of("x"))
      // f(x) -> (0, 0.0)
      val ps = t.positions

      ps.length must equalTo(2)
      ps must equalTo(Pos(List(0)) :: Pos(List(0,0)) :: Nil)
    }

    "implement positions for two arguments" in {
      val fs = FunctionSymbol("f", 2)
      val x = Var.of("x")
      // f(x,x) -> (0, 0.0, 0.1)
      val t: Term = Func(fs, x, x)

      val ps = t.positions

      ps.length must equalTo(3)
      ps must equalTo(Pos(List(0)) :: Pos(List(0,0)) :: Pos(List(0,1)) :: Nil)
    }

    "implement positions for two arguments" in {
      val f = FunctionSymbol("f", 2)
      val x = Var.of("x")
      // f(f(x,x),x) -> (0, 0.0, 0.1, 0.0.0, 0.0.1)
      val t: Term = Func(f, Func(f, x, x), x)

      val ps = t.positions

      ps.length must equalTo(5)
      ps must equalTo(Pos(List(0)) :: Pos(List(0,0))
        :: Pos(List(0,0,0)) :: Pos(List(0,0,1))
        :: Pos(List(0,1)) :: Nil)
    }

    "implement terms for no argument" in {
      val fs = FunctionSymbol("f", 0)
      val t: Term = Func(fs)
      val ts = t.terms

      ts.length must equalTo(1)
      ts.head must equalTo(t)
    }

    "implement terms for single argument" in {
      val f = FunctionSymbol("f", 1)
      val x = Var.of("x")
      val t: Term = Func(f, x)
      // f(x) -> (0, 0.0)
      val ts = t.terms

      ts.length must equalTo(2)
      ts must equalTo(t :: x :: Nil)
    }

    "implement terms for two arguments" in {
      val f = FunctionSymbol("f", 2)
      val x = Var.of("x")
      // f(x,x) -> (0, 0.0, 0.1)
      val t: Term = Func(f, x, x)

      val ts = t.terms

      ts.length must equalTo(3)
      ts must equalTo(t :: x :: x :: Nil)
    }

    "implement terms for two arguments" in {
      val f = FunctionSymbol("f", 2)
      val x = Var.of("x")
      // f(f(x,x),x) -> (0, 0.0, 0.1, 0.0.0, 0.0.1)
      val t: Term = Func(f, Func(f, x, x), x)

      val ts = t.terms

      ts.length must equalTo(5)
      ts must equalTo(t :: Func(f, x, x) :: x :: x :: x :: Nil)
    }

    "rewrite with empty substitution" in {
      val x = Var.of("x")
      val y = Var.of("y")
      val f = FunctionSymbol.of("f", 2)
      val t = Func(f, x, y)

      val subst = Map[Var, Term]()

      t.rewrite(subst) must equalTo(t)
    }

    "rewrite with disjoint substitution" in {
      val x = Var.of("x")
      val y = Var.of("y")
      val f = FunctionSymbol.of("f", 2)
      val t = Func(f, x, y)

      val subst = Map[Var, Term](Var.of("z") -> x)

      t.rewrite(subst) must equalTo(t)
    }

    "rewrite with actual substitution" in {
      val x = Var.of("x")
      val y = Var.of("y")
      val z = Var.of("z")
      val f = FunctionSymbol.of("f", 2)
      val t = Func(f, x, y)

      val subst = Map[Var, Term](x -> z)

      t.rewrite(subst) must equalTo(Func(f, z, y))
    }

    "rewrite with term (not var)" in {
      val x = Var.of("x")
      val y = Var.of("y")
      val f = FunctionSymbol.of("f", 2)
      val t = Func(f, x, y)
      val t2 = Func(f, y, x)

      val subst = Map[Var, Term](x -> t2)

      t.rewrite(subst) must equalTo(Func(f, t2, y))
    }

    "rewrite at several positions" in {
      val x = Var.of("x")
      val z = Var.of("z")
      val f = FunctionSymbol.of("f", 2)
      val t = Func(f, x, x)

      val subst = Map[Var, Term](x -> z)

      t.rewrite(subst) must equalTo(Func(f, z, z))
    }

  }
}
