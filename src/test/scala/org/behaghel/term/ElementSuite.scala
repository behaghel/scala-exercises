package org.behaghel.term

import org.scalatest.FunSuite
import Element.elem

class ElementSuite extends FunSuite {

  test("elem result should have passed width") {
    val ele = elem('w', 2, 3)
    assert(ele.width === 2)
  }

  test("elem result should have passed width again") {
    val ele = elem('w', 2, 3)
    expect(2){
      ele.width
    }
  }

  test("elem with negative width should throw IAE") {
    intercept[IllegalArgumentException]{
      elem('x', -2, 3)
    }
  }
}



// vim: set ts=2 sw=2 et:
