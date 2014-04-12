package org.behaghel.math

import org.scalatest.FunSuite

class MathematicaSuite extends FunSuite {
  
  import Mathematica._

  test("qsort should sort List[Int]") {
    assert(qsort(List(9, 6, 8, 11, 0, 5, 4)) === List(0, 4, 5, 6, 8, 9, 11))
  }

  test("qsort2 should sort List[Int]") {
    assert(qsort2(List(9, 6, 8, 11, 0, 5, 4)) === List(0, 4, 5, 6, 8, 9, 11))
  }

  test("powerSet given a set s should build a set of all subset of s") {
    assert(
      powerSet(Set('a', 'b', 'c')) === 
        Set(Set.empty[Char], Set('a'), Set('b'), Set('c'), 
          Set('a', 'b'), Set('b', 'c'), Set('a', 'c'), Set('a', 'b', 'c'))
    )
  }

  test("nestWhileList should produces list that include the start element") {
    assert(nestWhileList((_: Int) - 1, 10, (_: Int) > 0) contains 10)
  }

  test("nestWhileList should produces list that include the first element that doesn't match the predicate") {
    assert(nestWhileList((_: Int) - 1, 10, (_: Int) > 0) contains 0)
  }

}

// vim: set ts=2 sw=2 et:
