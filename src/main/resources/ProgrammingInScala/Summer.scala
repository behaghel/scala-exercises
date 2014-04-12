import ChecksumAccumulator.calculate

object Summer {

  def main(args: Array[String]) {
    for (arg <- args){
      val cs = calculate(arg)
      println(arg +": "+cs)
    }
  }
}
