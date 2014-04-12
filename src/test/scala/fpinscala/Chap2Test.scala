import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._
import Arbitrary.arbitrary
import fpinscala._


class BoxSpec extends Specification with ScalaCheck {

  implicit val ba = for { d1 <- arbitrary[Double]
                          d2 <- arbitrary[Double]
  } yield Box(d1, d2)

  "Box.taller" should {
    "always returns the box with the biggest height" in {
      Prop.forAll {
        (h1: Double, w1: Double, h2: Double, w2: Double) => (h1 > h2) ==> {
          val b1 = Box(h1, w1)
          val b2 = Box(h2, w2)
          Box.taller(b1, b2) == b1
        }
      }
    }
  }
}
