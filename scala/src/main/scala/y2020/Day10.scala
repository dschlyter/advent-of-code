package y2020
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Day10 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day10")

    val parsed = input.map(str => str.toLong).toVector.sorted
    val chain = Vector(0L) ++ parsed ++ Vector(parsed.max+3L)

    println(chain)

    var d1 = 0
    var d3 = 0

    println(chain(0))

    for (i <- chain.indices if i > 0) {
      val diff = (chain(i) - chain(i-1))
      diff match {
        case 1 => d1 += 1
        case 3 => d3 += 1
        case x => println("other diff", x)
      }
    }

    println(d1, d3)
    println("part1: " + d1 * d3)

    val m = mutable.Map[Long, Long]()
    m.put(0, 1)

    for (i <- chain.indices if i > 0) {
      val n = chain(i)
      val ways = m.getOrElse(n-1, 0L) + m.getOrElse(n-2, 0L) + m.getOrElse(n-3, 0L)
      m.put(n, ways)
      println(n, ways)
    }

    println("part2: " + m(chain.last))
  }
}
