package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day12 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day12")

    var data = input.map(str => (str.substring(0, 1), str.substring(1).toInt)).toVector

    {
      var direction = 0
      var pos = (0L, 0L)

      data.foreach(inst => {
        inst match {
          case ("N", x) => pos = (pos._1, pos._2 + x)
          case ("S", x) => pos = (pos._1, pos._2 - x)
          case ("E", x) => pos = (pos._1 + x, pos._2)
          case ("W", x) => pos = (pos._1 - x, pos._2)
          case ("L", x) => direction = (direction + x) % 360
          case ("R", x) => direction = (direction - x + 360) % 360
          case ("F", x) => pos = (
            pos._1 + math.round(math.cos(direction / 180.0 * math.Pi)) * x,
            pos._2 + math.round(math.sin(direction / 180.0 * math.Pi)) * x
          )
        }
        // println(inst, pos, direction)
      })

      println("part1", math.abs(pos._1) + math.abs(pos._2))
    }

    {
      var pos = (0L, 0L)
      var wp = (10L, 1L)

      data.foreach(inst => {
        inst match {
          case ("N", x) => wp = (wp._1, wp._2 + x)
          case ("S", x) => wp = (wp._1, wp._2 - x)
          case ("E", x) => wp = (wp._1 + x, wp._2)
          case ("W", x) => wp = (wp._1 - x, wp._2)
          case ("L", x) => Range(0, x / 90).foreach(_ => wp = (-wp._2, wp._1))
          case ("R", x) => Range(0, x / 90).foreach(_ => wp = (wp._2, -wp._1))
          case ("F", x) => pos = (pos._1 + wp._1 * x, pos._2 + wp._2 * x)
        }
        println(inst, pos, wp)
      })

      println("part2", math.abs(pos._1) + math.abs(pos._2))
    }
  }
}
