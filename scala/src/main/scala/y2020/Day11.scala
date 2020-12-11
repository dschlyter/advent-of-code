package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day11 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day11")
    Util.time("start")

    var startState = input.map(str => str.toCharArray.to(ArrayBuffer)).toVector

    {
      var last = startState
      var next = startState

      //pr(next)

      do {
        last = next
        next = step(last)
        //pr(next)
      } while (next != last)

      val occupied = next.map(_.count(x => x == '#')).sum
      println(occupied)
      Util.time("part1") // 1.46
    }

    {
      var last = startState
      var next = startState

      do {
        last = next
        next = step2(last)
        // pr(next)
      } while (next != last)
      val occupied = next.map(_.count(x => x == '#')).sum
      println(occupied)

      Util.time("part2") // 1.3
    }
  }

  def pr(s: Vector[ArrayBuffer[_]]): Unit = {
    for (row <- s) {
      for (c <- row) {
        print(c)
      }
      println()
    }
    println("-------")
  }

  def step(s: Vector[ArrayBuffer[Char]]): Vector[ArrayBuffer[Char]] = {
    val ret = Vector.fill(s.size)(mutable.ArrayBuffer[Char]())

    for (
      y <- s.indices;
      x <- s(y).indices
    ) {
      val adj = (for (
        yp <- -1 to 1;
        xp <- -1 to 1
        if !(xp == 0 && yp == 0)
      ) yield {
        val state = s.lift(y + yp).getOrElse(Vector()).lift(x + xp).getOrElse('.')
        state match {
          case '#' => 1
          case _ => 0
        }
      }).sum

      val curr = s.lift(y).getOrElse(Vector()).lift(x).getOrElse('.')
      val next = curr match {
        case 'L' if adj == 0 => '#'
        case '#' if adj >= 4 => 'L'
        case x => x
      }

      ret(y).append(next)
    }

    ret
  }

  def step2(s: Vector[ArrayBuffer[Char]]): Vector[ArrayBuffer[Char]] = {
    val ret = Vector.fill(s.size)(mutable.ArrayBuffer[Char]())

    for (
      y <- s.indices;
      x <- s(y).indices
    ) {
      val adj = (for (
        yp <- -1 to 1;
        xp <- -1 to 1
        if !(xp == 0 && yp == 0)
      ) yield {
        var offset = (yp, xp)
        var seen = '.'
        while (seen == '.') {
          seen = s.lift(y + offset._1).getOrElse(Vector()).lift(x + offset._2).getOrElse(' ')
          offset = (offset._1 + yp, offset._2 + xp)
        }
        seen match {
          case '#' => 1
          case _ => 0
        }
      }).sum

      val curr = s.lift(y).getOrElse(Vector()).lift(x).getOrElse('.')
      val next = curr match {
        case 'L' if adj == 0 => '#'
        case '#' if adj >= 5 => 'L'
        case x => x
      }

      ret(y).append(next)
    }

    ret
  }
}
