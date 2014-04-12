package org.behaghel.math

import scala.annotation.tailrec

/**
 * This has been done after this post: 
 *   http://thinkmeta.wordpress.com/2010/06/28/scala-expressiveness/
 */
object Mathematica {

  def qsort[A <% Ordered[A]](l: List[A]): List[A] = {
    l match {
      case Nil     => Nil
      case x :: xs => 
        val (lesser, greater) = xs partition ( _ < x )
        qsort(lesser) ++ List(x) ++ qsort(greater)
    }
  }

  def qsort2[A <% Ordered[A]](l: List[A]): List[A] = {
    l match {
      case Nil     => Nil
      case x :: xs => 
        qsort(for(y <- xs if y < x) yield y) ++ 
        List(x) ++ 
        qsort(for(y <- l if y > x) yield y)
    }
  }

  def powerSet[A](s: Set[A]) = {
    s.foldLeft(Set(Set.empty[A])) {
      (set, element) => set union (set map (_ + element))
    }
  }

  case class Power(base: Symbol, exponent: Int)
  
  def nestWhileList[A](f: (A) => A, start: A, hasNext: (A) => Boolean): List[A] = {
    val result = Stream.iterate(start)(f).takeWhile(hasNext).toList
    result :+ f(result.last)
  }

  val alg196 = (n: Int) => n + n.toString.reverse.toInt

  // this impl differs from the one in the post
  // this one has proven really more performant
  def isPalindrome[A](l: List[A]): Boolean = {
    l match {
      case Nil | _ :: Nil => true
      case xs => xs == xs.reverse
    }
  }

  def pascalTriangle(n: Int): List[List[Int]] = {
    def pascalTuple(l: List[Int]): List[Int] = l match {
      case Nil | _ :: Nil => l
      case x :: y :: xs   => x + y :: pascalTuple(y :: xs)
    }

    def nextPascalRow(l: List[Int]): List[Int] = {
      1 :: pascalTuple(l)
    }

    nestWhileList[List[Int]](nextPascalRow, 1 :: Nil, _.length < n)
  }

  def collatz(n: Int): Int = n match {
    case e if e % 2 == 0 => n / 2
    case o if o % 2 == 1 => 3 * n + 1
  }


  // this impl of bubbleSort differs from the one on the post
  // I find it more straightforward (well... not so if you knew how many 
  //  days^H^H^H^Htime I spent having it right)
  // In fact, I'm wondering if what the post calls suffixMatch 
  //  should rather be called prefixMatch
  @tailrec
  def bubbleSort[A <% Ordered[A]](l: List[A]): List[A] = {
  
    // bubbleSortRec returns None if nothing need to be done to sort l
    // else Some(unsorted, sorted) each time a permutation is required
    @tailrec
    def bubbleSortRec(l: List[A], passed: List[A] = Nil): Option[(List[A], List[A])] = {
      val bubble: PartialFunction[List[A], List[A]] = { case x :: y :: xs if x > y => y :: x :: xs }
      l match {
        case Nil                          => None
        case xs if bubble isDefinedAt xs  => Some((bubble(xs), passed))
        case x :: xs                      => bubbleSortRec(xs, passed :+ x)
      }
    }

    bubbleSortRec(l) match {
      case None => l
      case Some((xs, ys)) => bubbleSort(ys ++ xs)
    }
  }

  def runLengthEncoding[A](l: List[A]): List[(Int, A)] = {

    object *:: {

      def f(a: A, as: List[A], c: Int = 1): ((A, Int), List[A]) = as match {
        case Nil                => ((a, c), Nil)
        case x :: xs if x == a  => f(a, xs, c+1)
        case xs                 => ((a, c), xs)
      }

      def unapply(l: List[A]): Option[((A, Int), List[A])] = l match {
        case Nil      => None
        case x :: xs  => Some(f(x, xs))
      }

    }

    l match {
      case Nil            => Nil
      case (x, n) *:: xs  => (n, x) :: runLengthEncoding(l.drop(n))
    }

  }

}

// vim: set ts=2 sw=2 et:
