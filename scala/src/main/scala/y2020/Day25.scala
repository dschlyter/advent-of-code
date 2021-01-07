package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day25 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day25")

    Util.time("")

    part1(input)
    Util.time("part1")

    part2(input)
    Util.time("part2")
  }

  // Shared between part 1 and part2
  var tileColor = Map[(Int, Int), Boolean]()

  private def part1(input: Vector[String]) =  {
    val key1 = input(0).toLong
    val key2 = input(1).toLong

    var l1 = 0
    var sv = 1L
    while (sv != key1) {
      sv = (sv * 7) % 20201227
      l1 += 1
    }

    var res = 1L
    for (i <- Range(0, l1)) {
      res = (res * key2) % 20201227
    }

    println(res)
  }

  private def part2(input: Vector[String]) =  {

  }
}
