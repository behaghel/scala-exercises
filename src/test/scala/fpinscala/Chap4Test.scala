package fpinscala

import org.scalatest._
import Validation._

class Chap4Spec extends FlatSpec with Matchers  {
  "validation" should "support map" in {
    val xs = List(1, 2, 3, 4)
    assert {
      val v = validate(xs) { x =>
        x match {
          case i if i % 2 == 0 => Success(i)
          case y => Errors(List("odd " + y))
        }
      }
      v === Errors(List("odd 1", "odd 3"))
    }
    assert {
      val v = validate(xs) { x =>
        x match {
          case i if i < 10 => Success(i)
          case _ => Errors(List("."))
        }
      }
      v === Success(xs)
    }
  }
}