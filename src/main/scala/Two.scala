import scala.io.Source

object TwoPartOne extends App {
  override def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("day_2_input").getLines()
    val (twoTimes, threeTimes) = lines.foldLeft((0, 0)) { (acc, line) =>
      val a = line.groupBy(x=>x).mapValues(_.length)
      val l = if(a.values.exists(_ == 2 )) 1 else 0
      val r = if(a.values.exists(_ == 3 )) 1 else 0
      (acc._1+l, acc._2+r)
    }
    println(twoTimes * threeTimes)
  }
}

object TwoPartTwo extends App {
  override def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("day_2_input").getLines().toList

    def similarity(s1: String, s2: String) = {
      val sim = s1.zipWithIndex map{ case (c1, index) => if (c1 == s2.charAt(index)) 0 else 1 }
      (sim, sim.indexOf(1))
    }

    val threshold = 1

    for(line <- lines)
      for(t <- lines.tail) {
        val (similar, index) = similarity(line, t)
        if(similar.sum == threshold) {
          println(line diff line.charAt(index).toString)
          return
        }
      }
  }
}