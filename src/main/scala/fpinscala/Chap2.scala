package fpinscala

case class Box(height: Double, width: Double)

object Box {
  def wider(x: Box, y: Box): Box = greaterBy(x, y, _.width)

  def taller(x: Box, y: Box): Box = greaterBy(x, y, _.height)

  private def greaterBy(x: Box, y: Box, f: Box => Double): Box =
    if (f(x) > f(y)) x else y
}

object EasyMath {
  import FunctionUtils._
  def isEven(n: Int) = n % 2 == 0
  def isNegative(n: Int) = n < 0
  def not[A](f: A => Boolean): A => Boolean = a => !f(a)
  def isOdd = not(isEven)
  def isPositive = not(isNegative)
  def absolute[A](f: A => Int): A => Int = math.abs _ compose f
  def fib(n: BigInt): BigInt =
    if (n <= 1)
      n
    else {
      val r = fib(n-2)
      val s = fib(n-1)
      r + s
    }
  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n
    iterateWhile(2.0)(x => x - f(x) / (2 * x),
                      x => f(x).abs > 1e-14)
  }
}

object Predicate {
  import FunctionUtils._
  def divisibleBy(k: Int): Pred[Int] = _ % k == 0
  def isEven(n: Int) = divisibleBy(2)
  def isDivBy3And5 =
    //divisibleBy(3)(n) && divisibleBy(5)(n)
    lift[Int, Boolean, Boolean, Boolean](_ && _)(divisibleBy(3),
                                                 divisibleBy(5))
  def isDivBy3Or5 =
    //divisibleBy(3)(n) || divisibleBy(5)(n)
    lift[Int, Boolean, Boolean, Boolean](_ || _)(divisibleBy(3),
                                                 divisibleBy(5))
}

object FunctionUtils {

  type Pred[A] = A => Boolean

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => f(a, _)
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
  def lift[A, B, C, D](f: (B, C) => D)(g: A => B, h: A => C): A => D =
    a => f(g(a), h(a))
  def lift3[A, B, C, D, E](f: (B, C, D) => E)(g: A => B,
                                              h: A => C,
                                              i: A => D): A => E =
    //a => f(g(a), h(a), i(a))
    a => lift[A, C, D, E](f(g(a), _, _))(h, i)(a)
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A =
    if (!p(f(a))) a else iterateWhile(f(a))(f, p)

}
