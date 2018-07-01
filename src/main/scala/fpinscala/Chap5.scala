package fpinscala.async

import fpinscala.{List => _, Cons => _, Option => _}
import scala.{Stream => _}

sealed trait Stream[+A] {
  import Stream._
  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }
  def uncons: Option[(A, Stream[A])] = this match {
    case Cons(h, t) => Some(h(), t())
    case _ => None
  }
  def isEmpty: Boolean = headOption.isEmpty
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }
  def toListTailrec: List[A] = {
    @annotation.tailrec
    def go(xs: List[A], remaining: Stream[A]): List[A] =
      remaining match {
        case Cons(h, t) => go(h() :: xs, t())
        case _ => xs.reverse
      }
    go(Nil, this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(a, s) => f(a(), s().foldRight(z)(f))
    case _ => z
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(a, s) if n > 0 => cons(a(), s().take(n-1))
    case _ => Empty
  }
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, s) => if (p(a)) cons(a, s) else Empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
  def exists(p: A => Boolean): Boolean = foldRight(false)((a, z) => p(a) || z)

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A]){(a, o) => Some(a)}

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, s) => cons(f(a), s))
  def flatMap[B](f: A => Stream[B]): Stream[B] = this match {
    case Cons(a, s) => f(a()).foldRight(s() flatMap f)((x, y) => cons(x, y))
    case _ => Empty
  }
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, s) => if (f(a)) cons(a,s) else s)
  def append[AA >: A](a: => AA): Stream[AA] =
    foldRight(Stream(a))((aa, s) => cons(aa, s))

  def foldRight2[B, C](that: Stream[B], z: C)(f: (A, B, => C) => C): C = {
    (this, that) match {
      case (Cons(a, sa), Cons(b, sb)) => sa().foldRight2(sb(), f(a(), b(), z))(f)
      case _ => z
    }
  }
  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    this.foldRight2(that, empty[C])((a, b, c) => cons(f(a, b),c))
  def zip[B](that: Stream[B]): Stream[(A, B)] =
    this.zipWith(that)((a, b) => (a, b))
  override def equals(that: Any): Boolean = {
    if (that.isInstanceOf[Stream[A]]) {
      foldRight2(that.asInstanceOf[Stream[A]], true)((a, aa, b) => (a == aa) && b)
    }
    else false
  }

  // def find(p: A => Boolean) = filter(p).headOption
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Empty
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))
  def iterate[A](a: A)(f: A => A): Stream[A] = cons(a, iterate(f(a))(f))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = iterate(n)(_ + 1)
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a+b))
    go(0, 1)
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty[A]
    }

  // using unfold
  def fibsUnfold: Stream[Int] = unfold((0, 1)) {
    s => s match {
      case (a, b) => Some((a, (b, a+b)))
      case _ => None
    }
  }
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(n, n+1))
  def constantUnfold[A](a: A): Stream[A] = unfold(a)(s => Some(a, a))

}
