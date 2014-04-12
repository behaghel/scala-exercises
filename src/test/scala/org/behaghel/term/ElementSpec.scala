package org.behaghel.term

import org.scalatest._
import org.behaghel.term.Element.elem

class ElementSpec extends FlatSpec {
  "A UniformElement" should "have a width equal to the passed value" in {
      val ele = elem('x', 2, 3)
      assert(ele.width === 2)
  }
  it should "have a height equal to the passed value" in {
    val ele = elem('x', 2, 3)
    expect(3){ ele.height }
  }
  it should "throw an IAE if passed a negative width" in {
    intercept[IllegalArgumentException]{
      elem('x', -2, 4)
    }
  }
}


// vim: set ts=2 sw=2 et:
