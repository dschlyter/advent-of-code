package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day17 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day17")
    Util.time("")

    part1(input)
    Util.time("part1")

    part2(input)
    Util.time("part2")
  }

  private def part1(input: Vector[String]) =  {
    // Lazy way to init
    var cube = (for (
      x <- Range(-10, 20);
      y <- Range(-10, 20);
      z <- Range(-10, 20)
    ) yield {
      val active = z == 0 && input.lift(y).flatMap(line => line.lift(x).map(c => c == '#')).getOrElse(false)
      ((x,y,z), if (active) 1 else 0)
    }).toMap
    println("input", cube.values.count(_ == 1))

    for (cycle <- Range(0, 6)) {
      cube = cube.map(e => {
        val pos = e._1
        val state = e._2
        var activeNeigh = 0
        for (
          x <- Range(-1, 2);
          y <- Range(-1, 2);
          z <- Range(-1, 2)
          if x != 0 || y != 0 || z != 0
        ) {
          activeNeigh += cube.getOrElse((pos._1 + x, pos._2 + y, pos._3 + z), 0)
        }

        val newState = (state, activeNeigh) match {
          case (1, 2) => 1
          case (1, 3) => 1
          case (0, 3) => 1
          case _ => 0
        }

        (pos, newState)
      })
      println(cycle, cube.values.count(_ == 1))
    }
  }

  private def part2(input: Vector[String]) =  {
    // Lazy way to init
    var cube = (for (
      x <- Range(-10, 20);
      y <- Range(-10, 20);
      z <- Range(-10, 20);
      a <- Range(-10, 20)
    ) yield {
      val active = z == 0 && a == 0 && input.lift(y).flatMap(line => line.lift(x).map(c => c == '#')).getOrElse(false)
      ((x,y,z,a), if (active) 1 else 0)
    }).toMap
    println("input", cube.values.count(_ == 1))

    for (cycle <- Range(0, 6)) {
      cube = cube.map(e => {
        val pos = e._1
        val state = e._2
        var activeNeigh = 0
        for (
          x <- Range(-1, 2);
          y <- Range(-1, 2);
          z <- Range(-1, 2);
          a <- Range(-1, 2)
          if x != 0 || y != 0 || z != 0 || a != 0
        ) {
          activeNeigh += cube.getOrElse((pos._1 + x, pos._2 + y, pos._3 + z, pos._4 + a), 0)
        }

        val newState = (state, activeNeigh) match {
          case (1, 2) => 1
          case (1, 3) => 1
          case (0, 3) => 1
          case _ => 0
        }

        (pos, newState)
      })
      println(cycle, cube.values.count(_ == 1))
    }
  }
}
