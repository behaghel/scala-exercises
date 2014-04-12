package org.behaghel.term

import org.specs2.mutable._
import org.behaghel.term.Element.elem

class ElementSpecification extends Specification {
  
  "A UniformElement" should {
    "have a width equal to the passed value" in {
      val ele = elem('x', 2, 3)
      ele.width must be_==(2)
    }
    "have a height equal to the passed value" in {
      val ele = elem('x', 2, 3)
      ele.height must be_==(3)
    }
    "throw an IAE when passed a negative width" in {
      elem('x', -2, 4) must
        throwA[IllegalArgumentException]
    }
  }
}

// vim: set ts=2 sw=2 et:
