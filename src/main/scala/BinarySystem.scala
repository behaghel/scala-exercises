package org.behaghel

import scala.math.{log, ceil, floor, abs, pow}

object BinarySystem {

  def lg(x: Double): Double = log(x)/log(2)
  /**
   * return the closest positive integer for which pow(2, pow(2, x)) >= abs(i)
   * Useful to know the conventional size of the bit-word required to
   * store i, ie the smallest power of 2 that can be used as a word size to
   * store i in base 2.
   */
  def mag2(i: Int): Int = ceil(lg(lg(abs(i)))).toInt
  /** @return a Seq of 0 or 1 to represent i in base 2 using a
   * standard word size */
  def bin(i: Int): Seq[Int] = bin(i, pow(2, mag2(i) + 1).toInt) // +1 to deal with signed bit
  def bin(i: Int, n: Int): Seq[Int] = {
    require(n >= 0, "the number of bit must be positive.")
    if (i==0)
      List.fill(n)(0)
    else if (n==0)
      List(i) // ??? wtf?
    else if (i < 0)
      1 +: bin(pow(2, n-1).toInt + i, n-1)
    else
      if (i - pow(2, n-1) >= 0) 1 +: bin((i-pow(2,n-1)).toInt, n-1) else 0 +: bin(i, n-1)
  }
  def not(b: Int): Int = if (b==0) 1 else 0
  /**
   * complement of 2: flip any bit but the last 0s if any.
   * another way to say complement of 1 + 1.
   */
  def comp2(b: Seq[Int]): Seq[Int] = {
    val bs = b.reverse.takeWhile(_ < 1).reverse // foldRight should be
                                                // less ugly
    val bss = if (bs.length < b.length) 1 +: bs else bs
    b.take(b.length - bss.length).map(not) ++ bss
  }
}

object Sort {
  import BinarySystem.lg

  def insertionSort(is: Seq[Int]): Seq[Int] = {
    val a = is.toArray
    for (i <- 1 to a.length - 1) {
      val x = a(i)
      var j = i - 1
      while (j >= 0 && x < a(j)) {
        a(j+1) = a(j)
        j -= 1
      }
      a(j+1) = x
    }
    a
  }

  def mergeSort(is: Seq[Int]): Seq[Int] = mergeSort(is.toArray, 0, is.length-1)
  def mergeSort(a: Array[Int], p: Int, r: Int): Array[Int] = {
    if (p < r) {
      val i = (r + p) / 2
      mergeSort(a, p, i)
      mergeSort(a, i+1, r)
      merge(a, p, i, r)
    }
    a
  }
  def merge(a: Array[Int], p: Int, q: Int, r: Int): Array[Int] = {
    require(p <= q && q < r)

    var n1 = q - p + 1
    var n2 = r - q
    val a1 = a.drop(p).take(n1) :+ Int.MaxValue
    val a2 = a.drop(q+1).take(n2) :+ Int.MaxValue
    var i = 0
    var j = 0
    for (k <- p to r) {
      if (a1(i) <= a2(j)) {
        a(k) = a1(i)
        i += 1
      } else {
        a(k) = a2(j)
        j += 1
      }
    }
    a
  }
  trait ArrayHeap {
    def underlying: Array[Int]
    def heapify(i: Int)
    def optimum: Int = underlying(0)
    private var _size: Int = _
    def size: Int = _size
    private[behaghel] def size_=(s: Int) { _size = s }
    def extractOptimum: Int = {
      val x = optimum
      size -= 1
      underlying(0) = underlying(size)
      heapify(0)
      x
    }
    protected def sorted(i: Int, j: Int): Boolean
    def insert(n: Int) {
      import Heap.parent
      // XXX extend underlying if necessary
      underlying(size) = n
      var i = size
      size += 1
      while(parent(i) > 0 && sorted(underlying(parent(i)), underlying(i))) {
        underlying(i) = underlying(parent(i))
        underlying(parent(i)) = n
        i = parent(i)
      }
    }
  }

  class MinHeap(val underlying: Array[Int]) extends ArrayHeap {
    def min: Int = optimum
    def extractMin: Int = extractOptimum
    def heapify(i: Int) = Heap.minHeapify(underlying, i, size)
    def sorted(i: Int, j: Int) = i <= j
  }
  class MaxHeap(val underlying: Array[Int]) extends ArrayHeap {
    def max: Int = optimum
    def extractMax: Int = extractOptimum
    def heapify(i: Int) = Heap.maxHeapify(underlying, i, size)
    def sorted(i: Int, j: Int) = i >= j
  }
  object Heap {
    def parent(i: Int) = i / 2
    def left(i: Int) = 2 * i
    def right(i: Int) = 2 * i + 1
    def minHeapify(a: Array[Int], i: Int, heapSize: Int) {
      val l = left(i)
      val r = right(i)
      var smallest = Int.MaxValue
      if (l < heapSize && a(i) > a(l)) {
        smallest = l
      } else smallest = i
      if (r < heapSize && a(smallest) > a(r)) {
        smallest = r
      }
      if (i != smallest) {
        val tmp = a(i)
        a(i) = a(smallest)
        a(smallest) = tmp
        minHeapify(a, smallest, heapSize)
      }
    }
    def buildMinHeap(a: Array[Int], size: Int): MinHeap = {
      require(a.length >= size)
      for(i <- (size/2) to 0 by -1)
        minHeapify(a, i, size)
      val h = new MinHeap(a)
      h.size = size
      h
    }
    def maxHeapify(a: Array[Int], i: Int, heapSize: Int) {
      val l = left(i)
      val r = right(i)
      var largest = Int.MinValue
      if (l < heapSize && a(i) < a(l)) {
        largest = l
      } else largest = i
      if (r < heapSize && a(largest) < a(r)) {
        largest = r
      }
      if (i != largest) {
        val tmp = a(i)
        a(i) = a(largest)
        a(largest) = tmp
        maxHeapify(a, largest, heapSize)
      }
    }
    def buildMaxHeap(a: Array[Int], size: Int): MaxHeap = {
      require(a.length >= size)
      for(i <- (size/2) to 0 by -1)
        maxHeapify(a, i, size)
      val h = new MaxHeap(a)
      h.size = size
      h
    }
  }

  def heapSort(is: Seq[Int]): Seq[Int] = {
    val n = is.length
    val heap = Heap.buildMaxHeap(is.toArray, n)
    Array.tabulate(n)(_ => heap.extractMax).toSeq.reverse
  }

}
