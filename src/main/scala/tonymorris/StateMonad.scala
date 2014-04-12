package tonymorris

import language.{ higherKinds, postfixOps }

// if you memoize fib with an immutable map you'll be disappointed
// it will fork your persistent map requiring memoizing some value
// multiple times (aka alzheimemoization)
import collection.mutable.Map

object NaiveFibonacci {
  def fib(n: BigInt): BigInt =
    if (n <= 1)
      n
    else
      fib(n - 2) + fib(n - 1)
}

object MemoFibonacci {
  type Memo = Map[BigInt, BigInt]

  def fib(n: BigInt): BigInt = {
    def fibMemo(n: BigInt, m: Memo = Map()): (BigInt, Memo) =
      if (n <= 1)
        (n, m)
      else m get n match {
        case None =>
          val (r, mm) = fibMemo(n - 2, m)
          val (s, mmm) = fibMemo(n - 1, mm)
          (r + s, mmm)
        case Some(x) => (x, m)
      }
    fibMemo(n)._1
  }
}

case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, ss) = run(s)
    (f(a), ss)
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, ss) = run(s)
    f(a) run ss
  }
  // convenience function to drop the resulting state value
  def eval(s: S): A = run(s)._1
}
object State {
  def insert[S, A](a: A): State[S, A] = State[S, A] { s => (a, s) }
  def memo[A, B](a: A, f: Map[A, B] => B): State[Map[A, B], B] = State { map =>
    map get a match {
      case None =>
        val r = f(map)
        (r, map += (a -> r))
      case Some(v) => (v, map)
    }
  }
}

object StateMonadFib {
  type Memo = Map[BigInt, BigInt]
  def fib(n: BigInt): BigInt = {
    def fibMemoM(n: BigInt): State[Memo, BigInt] =
      State.memo(n, s => {
                   if (n <= 1)
                     n
                   else {
                     val k = for { u <- fibMemoM(n-1)
                                   v <- fibMemoM(n-2)
                     } yield u + v
                     k eval s
                   }
                 }
      )
    fibMemoM(n).eval(Map())
  }
}

object StateMemoFib {
  type Memo = Map[BigInt, BigInt]
  def fib(n: BigInt): BigInt = {
    def fibMemoS(n: BigInt): State[Memo, BigInt] =
      State (memo =>
        if (n <= 1)
           (n, memo)
        else memo get n match {
          case None =>
            val (s, m) = fibMemoS(n - 2) run memo
            val (t, mm) = fibMemoS(n - 1) run m
            val r = s + t
            (r, mm + (n -> r))
          case Some(r) => (r, memo)
        })
    fibMemoS(n) run Map() _1
  }
}
