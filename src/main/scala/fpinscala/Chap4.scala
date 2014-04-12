package fpinscala

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](otherwise: => Option[B]): Option[B]
  def filter(p: A => Boolean): Option[A]
  def isEmpty: Boolean
}
case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None
  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  override def getOrElse[B](default: => B): B = default
  override def orElse[B](otherwise: => Option[B]): Option[B] = otherwise
  override def filter(p: Nothing => Boolean): Option[Nothing] = None
  override def isEmpty: Boolean = true
}
case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
  override def getOrElse[B >: A](default: => B): B = get
  override def orElse[B >: A](default: => Option[B]): Option[B] = this
  override def filter(p: A => Boolean): Option[A] = if (p(get)) this else None
  override def isEmpty: Boolean = false
}
object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])
          (f: (A, B) => C): Option[C] =
    for { aa <- a
          bb <- b
    } yield f(aa, bb)
  def sequence[A](xs: List[Option[A]]): Option[List[A]] = traverse(xs)(identity)

  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] =
    xs match {
      case Nil => Some(Nil)
      case Cons(a, as) =>
        for { b <- f(a)
              bs <- traverse(as)(f)
        } yield Cons(b, bs)
    }
}

object EasyStats {
  def mean(xs: List[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(List.sum(xs)/List.length(xs))
  def variance(xs: List[Double]): Option[Double] =
    mean(xs) flatMap { m => mean(List.map(xs)(x => math.pow(x - m, 2))) }
}

object Regexp {
  import java.util.regex._
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }
  def matchit(p: Pattern)(s: String): Boolean = p.matcher(s).matches
  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map matchit
  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    Option.map2(pattern(pat), pattern(pat2)){
      (p1, p2) => matchit(p1)(s) && matchit(p2)(s)
    }
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E,B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for { a <- this
          b <- that
    } yield f(a, b)
  def orElse[EE >: E, B >: A](otherwise: => Either[EE, B]): Either[EE, B]
}

case class Left[+E](e: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Either[E,B] = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]):
      Either[EE, B] = this
  override def orElse[EE >: E, B](otherwise: => Either[EE, B]):
      Either[EE, B] = otherwise
}

case class Right[+A](a: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing,B] = Right(f(a))
  override def flatMap[Nothing, B](f: A => Either[Nothing, B]):
      Either[Nothing, B] = f(a)
  override def orElse[Nothing, B >: A](otherwise: => Either[Nothing, B]):
      Either[Nothing, B] = this
}

object Either {
  def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    xs match {
      case Nil => Right(Nil)
      case Cons(a, as) =>
        for { b <- f(a)
              bs <- traverse(as)(f)
        } yield Cons(b, bs)
    }
  def sequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] =
    traverse(xs)(identity)
}
