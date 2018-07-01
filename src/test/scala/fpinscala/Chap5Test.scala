package fpinscala.async

import org.scalatest._

class Chap5Spec extends FlatSpec with Matchers  {
  val st = Stream(1, 2, 3, 4, 5)
  "Stream#equals" should "say true on equality" in {
    assert(st === Stream(1, 2, 3, 4, 5))
  }
  it should "say true on identity" in {
    assert(st === st)
  }
  it should "say false otherwise" in {
    assert(st != Stream('a'))
    assert(st != "a")
  }
  "Stream#take" should "collect n first elements" in {
    assert(st.take(2) === Stream(1, 2))
  }
  it should "stop when no more elements in the Stream" in {
    assert(st.take(1000) === st)
  }
  "Stream#takeWhile" should "collect first elements that matches p" in {
    assert(st.takeWhile(_ < 2) === Stream(1))
  }
  it should "return empty when head doesn't match" in {
    assert(st.takeWhile(_ > 3) === Stream.empty[Int])
  }
  "Stream#forAll" should "terminate as soon as it encounters a nonmatching value" in {
    // import scala.collection.mutable.ListBuffer
    // val b = new ListBuffer
    var c = 0
    def inc() { c += 1 }
    val s = Stream.iterate(1)(i => {inc(); i + 1})
    assert(s.forAll(_ < 3) == false)
    assert(c <= 3)
  }
  "Stream#headOption" should "return Some(head) when non-empty" in {
    assert(st.headOptionViaFoldRight === Some(1))
  }
}