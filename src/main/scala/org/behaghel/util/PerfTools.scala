package org.behaghel.util

object PerfTools {
  
  def time[A](x: => A) = {
    val start = java.lang.System.currentTimeMillis
    val result = x
    println("duration: " + (java.lang.System.currentTimeMillis - start) + "ms")
    result
  }

  def avgTime[A](x: => A, sample: Int = 100) = {
    import scala.collection.mutable.ListBuffer
    val l: ListBuffer[Long] = new ListBuffer
    for (i <- 1 until sample) {
      val start = java.lang.System.currentTimeMillis
      x
      val d = (java.lang.System.currentTimeMillis - start)
      l += d
    }
    val cumulTime = l.toList.foldLeft(0L)((_: Long) + (_: Long))
    val avgTime = cumulTime / sample
    println("average duration on "+ sample +" executions: "+ avgTime +"ms")
    x
  }


}

// vim: set ts=2 sw=2 et:
