package org.behaghel.term

import org.scalacheck._
import org.behaghel.term.Element.elem

object ElementPropertiesSuite extends Properties("Element") {
  import Prop._

  property("width") =
    forAll {
      w: Int => 
        (w > 0 && w < 1e9) ==> (elem('x', w, 3).width == w)
    }
  
  property("height") =
    forAll {
      h: Int => 
        (h > 0) ==> (elem('x', 2, h).height == h)
    }

  
}

// vim: set ts=2 sw=2 et:
