package y2020

object Day1 {
  def main(args: Array[String]): Unit = {
    val data = Util.readInput("day1").map(_.toInt)

    val part1 = for {
      x <- data
      y <- data
      if x + y == 2020
    } yield x*y

    println(part1)

    val part2 = for {
      x <- data
      y <- data
      z <- data
      if x + y + z == 2020
    } yield x*y*z

    println(part2)
  }
}
