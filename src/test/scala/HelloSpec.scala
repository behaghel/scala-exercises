import org.scalatest._
import Matchers._

class HelloSpec extends FlatSpec with Matchers {
  "Hello" should "have tests" in {
    true shouldEqual true
  }
}
