package org.behaghel.expr

import org.scalatest._
import org.behaghel.expr.Expr._

class ExprScalaTest extends FlatSpec  with Matchers {
  "simplifyTop" should "simplify double negation" in {
      simplifyTop(UnOp("-", UnOp("-", Number(4)))) should be (Number(4))
  }
  it should "simplify when adding zero" in {
    pending
  }
  it should "simplify multiplying by one" in {
    pending
  }
}

// vim: set ts=2 sw=2 et:
