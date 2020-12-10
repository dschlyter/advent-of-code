package y2020
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Day9 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day9")

    val parsed = input.map(str => str.toLong).toVector
    var ans: Long = 0

    // part 1
    // TODO this could be much nicer
    breakable {
      for (i <- 25 until parsed.size) {
        var found = false

        for (
          j <- (i-25).until(i);
          k <- (j+1).until(i)
          if parsed(j) + parsed(k) == parsed(i)
        ) {
          found = true
        }

        if (!found) {
          ans = parsed(i)
          println(parsed(i))
          break
        }
      }
    }

    // part 2
    breakable {
      for (
        start <- parsed.indices;
        stop <- (start+2).until(parsed.size)
      ) {
        val slice = parsed.slice(start, stop)

        if (slice.sum == ans) {
          println(slice.min + slice.max)
          break
        }
      }
    }
  }
}
