import scala.io.Source

object OnePartOne extends App {
  override def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("day_1_input").getLines()
    val result = lines.foldLeft(0){ (acc, line) =>
      line.charAt(0) match {
        case '-' => acc - line.substring(1).toInt
        case '+' => acc + line.substring(1).toInt
      }
    }
    println(result)
  }
}

object OnePartTwo extends App {
  override def main(args: Array[String]): Unit = {
    def rec(history: List[Int], lines: List[String], acc: Int): Int = {
      val line = lines.head
      val diff = line.charAt(0) match {
        case '-' => acc - line.substring(1).toInt
        case '+' => acc + line.substring(1).toInt
      }
      if(history contains diff) return diff
      if(lines.size == 1) rec(history :+ diff, Source.fromFile("day_1_input").getLines().toList, diff)
      else rec(history :+ diff, lines.tail, diff)
    }
    println(rec(List(0), Source.fromFile("day_1_input").getLines().toList, 0))
  }
}