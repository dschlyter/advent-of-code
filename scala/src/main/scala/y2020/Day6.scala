package y2020
import scala.collection.{mutable => m}

object Day6 {
  def main(args: Array[String]): Unit = {
    val input = Util.groupedInput("day6")

    // part 1
    val part1 = input.map(group => {
      val allChars = group.map(line => line.toCharArray.toSet).reduce((a, b) => a ++ b)
      allChars.size
    }).sum

    println(part1)

    // part 2
    val part2 = input.map(group => {
      val allChars = group.map(line => line.toCharArray.toSet).reduce((a, b) => a.intersect(b))
      allChars.size
    }).sum

    println(part2)
  }
}
