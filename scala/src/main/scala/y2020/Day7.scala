package y2020
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Day7 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day7")

    // part 1
    val parsed = input.flatMap(str => {
      val x = str.replace("bags", "").replace("bag", "").replace(".", "")
      val s = x.split("contain").toList
      val h = s.head.trim

      if (!(s(1).contains("no other"))) {
        val t = s(1).trim.split(",").toList
        val r = t.map(snd => {
          val s2 = snd.trim.split(" ", 2).toList
          (s2(1), s2.head.toInt)
        })
        Some((h, r))
      } else {
        None
      }

    })


    val m = parsed.toMap
    var r = mutable.Map[String, Set[String]]()
    m.foreach{ case (outerBag, bags) =>
      bags.foreach{case (innerBag, count) => r(innerBag) = r.getOrElse(innerBag, Set()) + outerBag}
    }

    {
      val q = mutable.Queue[String]()
      var seen = Set[String]()
      var count = 0

      var first = true

      q.append("shiny gold")

      while (q.nonEmpty) {
        val h = q.dequeue()
        if (!seen.contains(h)) {
          if (!first) {
            seen = seen + h
            count += 1
          }
          first = false
          // r.getOrElse(h, List()).foreach{ case (bag, count) => q.append(bag) }
          r.get(h).foreach(bags => bags.foreach(q.append))
        }
      }

      println(count)
    }

    // part 2

    def inside(bag: String): Long = {
      m.getOrElse(bag, List()).map(b => b._2 + b._2 * inside(b._1)).sum
    }
    println(inside("shiny gold"))
  }
}
