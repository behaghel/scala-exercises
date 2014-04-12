package org.behaghel.expr

import org.specs2.mutable._
import org.behaghel.expr.Expr._

class ExprTest extends Specification {
  "simplifyTop" should {
    "simplify double negation" in {
      simplifyTop(UnOp("-", UnOp("-", Number(4)))) must_== Number(4)
    }
    "simplify when adding zero" in {
      simplifyTop(BinOp("+", Number(0), Number(4))) must_== Number(4)
    }
    "simplify multiplying by one" in {
      simplifyTop(BinOp("*", Number(1), Number(4))) must_== Number(4)
    }
  }
}

// vim: set ts=2 sw=2 et:
