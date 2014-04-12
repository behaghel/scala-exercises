package fpinscala

sealed trait List[+A] {
  def head: A
  def tail: List[A]
  def isEmpty: Boolean
}
case object Nil extends List[Nothing] {
  override def tail = sys.error("Empty list")
  override def head = sys.error("Empty list")
  override def isEmpty = true
}
case class Cons[A](head: A, tail: List[A]) extends List[A] {
  override def isEmpty = false
}

object List {
  def apply[A](xs: A*): List[A] =
    if (xs.isEmpty) Nil else Cons(xs.head, apply(xs.tail: _*))
  def drop[A](xs: List[A], n: Int): List[A] = {
    require(n>=0)
    xs match {
      case Nil => Nil
      case as if n ==0 => as
      case as if n == 1 => as.tail
      case as => drop(as.tail, n - 1)
    }
  }
  def dropWhile[A](xs: List[A])(p: A => Boolean): List[A] =
    xs match {
      case Nil => Nil
      case Cons(a, as) if p(a) => dropWhile(as)(p)
      case _ => xs
    }
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => Cons(a, Nil)
    case Cons(x, xs) => Cons(a, xs)
  }
  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
  def foldRight[A,B](as: List[A], b: B)(f: (A, B) => B): B =
    as match {
      case Nil => b
      case Cons(x, xs) => f(x, foldRight(xs, b)(f))
    }
  def sum(is: List[Double]): Double = foldLeft(0.0, is)(_ + _)
  def product(ds: List[Double]): Double = foldLeft(1.0, ds)(_ * _)
  def length[A](xs: List[A]): Int = foldLeft(0, xs)((i, x) => i + 1)
  def foldLeft[A, B](b: B, as: List[A])(f: (B, A) => B): B =
    as match {
      case Nil => b
      case Cons(x, xs) => foldLeft(f(b, x), xs)(f)
    }
  // scala> def foldLeft[A, B](z: B, xs: List[A])(f: (B, A) => B) =
  //            List.foldRight(List.reverse(xs), z)((a, b) => f(b, a))
  // scala> def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B) =
  //            List.foldLeft(z, List.reverse(xs))((b, a) => f(a, b)
  def append[A](xs: List[A], x: A) =
    foldRight(xs, Cons(x, Nil))(Cons(_, _))
  def reverse[A](xs: List[A]): List[A] =
    List.foldRight(xs, Nil: List[A])((a, rs) => append(rs, a))
  def flatten[A](xs: List[List[A]]): List[A] =
    xs match {
      case Nil => Nil
      case Cons(y, ys) => foldRight(y, flatten(ys))(Cons(_, _))
    }
  // {
  //   def _flatten(xs: List[List[A]], as: List[A]): List[A] =
  //     xs match {
  //       case Nil => as
  //       case Cons(y, ys) => _flatten(ys, foldRight(as, y)(Cons(_, _)))
  //     }
  //   _flatten(xs, Nil: List[A])
  // }
  def map[A, B](xs: List[A])(f: A => B): List[B] =
    foldRight(xs, Nil: List[B])((a, rs) => Cons(f(a), rs))
  def filter[A](xs: List[A])(p: A => Boolean): List[A] =
    flatMap(xs)(x => if (p(x)) List(x) else Nil)
  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
    flatten(map(xs)(f))
  def get[A](xs: List[A], n: Int): A = drop(xs, n).head
  def zip[A, B](xs: List[A], ys: List[B]): List[(A, B)] =
    List((0 to (math.min(length(xs), length(ys))-1)) map { i =>
      (get(xs, i), get(ys, i))}: _*)
  def zipMap[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] =
    map(zip(xs, ys))({ case (a, b) => f(a, b) })
  def hasPrefix[A](xs: List[A], ys: List[A]): Boolean =
    zip(xs, ys) match {
      case Nil => true
      case Cons((a, b), rs) if a == b =>
        hasPrefix(xs.tail, ys.tail)
      case _ => false
    }
  def hasSubsequence[A](xs: List[A], ys: List[A]): Boolean =
    ys.isEmpty || {
      val ls = dropWhile(xs)(_ != ys.head)
      !ls.isEmpty && (hasPrefix(ls, ys) || hasSubsequence(ls.tail, ys))
    }
}

sealed trait Tree[+A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int =
    fold(t)({ _ => 0 }, { (l, r) => 1 + size(l) + size(r) })
    // t match {
    //   case Leaf(_) => 0
    //   case Node(l, r) => 1 + size(l) + size(r)
    // }
  def maximum(t: Tree[Int]): Int =
    fold(t)({ i => i }, { (l, r) => math.max(maximum(l), maximum(r)) })
    // t match {
    //   case Leaf(i) => i
    //   case Node(l, r) => math.max(maximum(l), maximum(r))
    // }
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)({ a => Leaf(f(a)) }, { (l, r) => Node(map(l)(f), map(r)(f)) })
    // t match {
    //   case Leaf(a) => Leaf(f(a))
    //   case Node(l, r) => Node(map(l)(f), map(r)(f))
    // }
  def depth[A](t: Tree[A]): Int =
    fold(t)({ a => 1 }, { (l,r) => 1 + math.max(depth(l), depth(r)) })
  // t match {
  //   case Leaf(_) => 1
  //   case Node(l, r) => 1 + math.max(depth(l), depth(r))
  // }
  def fold[A, B](t: Tree[A])(leaf: A => B,
                             node: (Tree[A], Tree[A]) => B): B =
    t match {
      case Leaf(x) => leaf(x)
      case Node(l, r) => node(l, r)
    }
}
