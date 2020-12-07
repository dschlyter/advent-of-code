package y2020
import scala.collection.{mutable => m}

object Day5 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day5")

    // part 1
    val data = input.map(line => {
      val row = line.substring(0, 7).replace("F", "0").replace("B", "1")
      val col = line.substring(7, 10).replace("L", "0").replace("R", "1")

      (Integer.parseInt(row, 2), Integer.parseInt(col, 2))
    })

    val ids = data.map{case (r, c) => r*8 + c}.toSet
    val biggest = ids.max
    val smallest = ids.min
    println("part1", biggest)

    println("part2", Range(smallest, biggest).filter(i => !ids.contains(i)))
  }
}
