package org.behaghel.math.euler

object Euler {

  // Find the sum of all the multiples of 3 or 5 below 1000.
  lazy val ex1 = (3 to 999).filter(i => i % 5 == 0 || i % 3 == 0).foldLeft(0)(_ + _)

  // Find the sum of first 4000000 even fibonacci numbers.
  // fibonacci stream
  def fibFrom(a: Int, b: Int): Stream[Int] = a #:: fibFrom(b, a+b)
  lazy val ex2 = 
    fibFrom(1, 2).takeWhile(_ < 4000000).filter(_ % 2 ==0).foldLeft(0)(_ + _)

  // Find the largest prime factor of the number 600851475143.
  def primeFactorsInterval(i: Long) = 2L to (math.sqrt(i).toLong + 1L)

  def isPrime(i: Long): Boolean = i == 2 || primeFactorsInterval(i).forall(i % _ != 0)

  def maxPrimeFactor(i: Long): Option[Long] = 
    primeFactorsInterval(i).reverse.find(j => i % j == 0 && isPrime(j))
  lazy val ex3 = maxPrimeFactor(600851475143L)

  // Find the largest palindrome made from the product of two 3-digit numbers.
  val r = (100 until 1000).reverse
  val xs = 
    for (i <- r;
         j <- r; 
         p = i * j if p.toString.reverse.toInt == p)
      yield p
  lazy val ex4 = xs.sortWith(_ > _).head

  // Find the smallest positive number that is evenly divisible 
  // by all of the numbers from 1 to 20.
  def divByAll(i: Long, ds: Seq[Long]): Boolean = ds.forall(i % _ == 0)

  lazy val ex5 = {
    var i = 2
    val ds = 1L to 20L
    while (!divByAll(i, ds))
      i += 2 // 2 to go faster: what we are looking for is necessarily even
    i
  }

  // Find the difference between 
  // the sum of the squares of the first one hundred natural numbers 
  // and the square of the sum.
  import math.pow
  def sumFromOneTo(n: Int): Int = n * (n + 1) / 2
  lazy val ex6a = pow(sumFromOneTo(100), 2)
  lazy val ex6b = (1 to 100).map{ pow(_, 2) }.foldLeft(0D){ _ + _ }
  lazy val ex6 = ex6a - ex6b

  // Find the 10001st prime number.
  def nextPrime(i: Long): Long = if (isPrime(i)) i else nextPrime(i+1)
  lazy val ex7 = Stream.iterate(2L){ (j: Long) => nextPrime(j + 1) }.drop(10000).head

  val ex8Int = """
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
"""


  def solve8(s: String) = {
    def str2ai(s: String): Array[Int] = {
      val ls = io.Source.fromString(s).getLines
      val l = for (l <- ls; if l.length > 0) yield l.toArray map (_.toString.toInt)
      l.toArray.flatten
    }

    str2ai(ex8Int).sliding(5).foldLeft(0) { (m, ls) => math.max(m, ls.reduceLeft { _ * _ }) }
  }
  
  lazy val ex8 = solve8(ex8Int)

  lazy val ex9 = 
    for (i <- 1 to 333; 
         j <- i to 499; 
         k = math.sqrt(i*i+j*j) if k == k.toInt && i+j+k == 1000) 
      yield i*j*k.toInt
  
  // Find the greatest product of four number in a row in whatever direction inside
  // a 20x20 matrix.
  val ex11Grid = """
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48  
"""
  def str2Matrix(s: String) = {
    val ls = io.Source.fromString(ex11Grid).getLines
    val ia = for (l <- ls; if l.length>0) yield l.split(" ").map(_.toInt)
    ia.toArray
  }
  val matrix = str2Matrix(ex11Grid)
  def m(p: (Int, Int)) = matrix(p._1)(p._2)

  implicit def pairIntInt2Point(p: (Int, Int)) = Point(p)

  case class Point(p: (Int, Int)) {

    def +(v: (Int, Int)) = (p._1 + v._1, p._2 + v._2)

    private def collect(p: (Int, Int), v: (Int, Int), n: Int) = {

      @scala.annotation.tailrec
      def _collect(l: List[(Int, Int)], v: (Int, Int), n: Int): List[(Int, Int)] =
        if (n == 0) l else _collect((l.head + v) :: l, v, n - 1)

      _collect(p :: Nil, v, n)
    }

    def | = collect(p, (1, 0), 3)

    def \ = collect(p, (1, 1), 3)

    def / = collect(p, (1, -1), 3)

    def - = collect(p, (0, 1), 3)
  }

  def segments(p: (Int, Int)) = p match {
    case (i, j) if i > 16 && j > 16 => Nil
    case (i, _) if i > 16           => p.- :: Nil
    case (_, j) if j < 3            => p.| :: p.\ :: p.- :: Nil
    case (_, j) if j > 16           => p./ :: p.| :: Nil
    case _                          => p./ :: p.| :: p.\ :: p.- :: Nil
  }

  val pos = for (i <- 0 to 19; j <- 0 to 19) yield (i, j)
  lazy val ex11 = pos flatMap segments map { _.foldLeft(1) { (a, b) => a * m(b) } } reduceLeft math.max

}
