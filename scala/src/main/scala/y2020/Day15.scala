package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day15 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day15")

    val parsed = input(0).split(",").map(line => line.toInt).toVector


    {
      val m = mutable.ArrayBuffer.from(parsed)
      while (m.length < 2020) {
        val lastSpoken = m.last
        var start = -1
        var diff = 0
        for ((n, i) <- m.zipWithIndex) {
          if (n == lastSpoken) {
            if (start != -1) {
              diff = i - start
            }
            start = i
          }
        }
        m.append(diff)
      }
      println(m.last)
    }

    {
      val last = mutable.Map[Int, Int]()
      val last2 = mutable.Map[Int, Int]()

      for ((n, i) <- parsed.zipWithIndex) {
        last(n) = i
      }

      var count = parsed.length
      var spoken = parsed.last

      while (count < 30000000) {
        spoken = last2.get(spoken) match {
          case None => 0
          case Some(x) => count - 1 - x
        }

        last.get(spoken).foreach(l2 => last2(spoken) = l2)
        last(spoken) = count
        count += 1
      }
      println(spoken)
    }
  }
}
