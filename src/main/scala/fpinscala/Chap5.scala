package fpinscala

trait Stream[+A] {
  import Stream._
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def toList: List[A] =
    uncons map { o => Cons(o._1, o._2.toList) } getOrElse Nil
  def take(n: Int): Stream[A] = uncons match {
    case Some((a, s)) if n > 0 => cons(a, s.take(n-1))
    case _ => empty
  }
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, s) => if (p(a)) cons(a, s) else empty)
  def foldRight[B, C](z: B)(f: (A, => B) => B): B = uncons match {
    case None => z
    case Some((a, s)) => f(a, s.foldRight(z)(f))
  }
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, s) => cons(f(a), s))
  def flatMap[B](f: A => Stream[B]): Stream[B] = uncons match {
    case None => empty
    case Some((a, s)) => f(a).foldRight(s flatMap f)((a, b) => cons(a, b))
  }
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, s) => if (f(a)) cons(a,s) else s)
  def append[AA >: A](a: AA): Stream[AA] =
    foldRight(Stream(a))((aa, s) => cons(aa, s))
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, z) => p(a) || z)
}

object Stream {
  def empty[A]: Stream[A] = new Stream[A] { def uncons = None }
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
