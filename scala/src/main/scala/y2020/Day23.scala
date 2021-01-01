package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day23 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day23")

    Util.time("")

    part1(input)
    Util.time("part1")

    part2(input)
    Util.time("part2")
  }

  private def part1(input: Vector[String]) =  {
    var cups = input.head.map(_.toString.toInt).toVector
    val cupCount = cups.length

    println(cups)

    for (move <- Range(0, 100)) {
      val pickUp = cups.slice(1,4)
      val destPos = Range(-1, -cups.length, -1)
        .map(cups(0) + _)
        .map(i => if (i <= 0) {
          cups.max + i
        } else {
          i
        })
        .map(cups.indexOf(_))
        .filter(_ >= 4)
        .head
      val dest = cups(destPos)
      cups = cups.slice(4, destPos+1) ++ pickUp ++ cups.slice(destPos+1, cups.length) ++ cups.slice(0, 1)

      assert(cups.length == cupCount)
      // println(cups.mkString, cups.last, pickUp, dest, cups(0))
    }

    val cupOne = cups.indexOf(1)
    cups = cups.slice(cupOne+1, cups.size) ++ cups.slice(0, cupOne)

    println(cups.mkString)
  }

  private def part2(input: Vector[String]) =  {
    var cups = input.head.map(_.toString.toInt).toVector
    cups = cups ++ (Range(cups.max+1, 1000001).toVector)
    assert(cups.length == 1000000)

    val next = mutable.Map() ++ cups.zipWithIndex.map{case (n, index) => (n, cups((index+1) % cups.length))}.toMap
    var curr = cups(0)

    for (move <- Range(0, 10_000_000)) {
      val pick1 = next(curr)
      val pick2 = next(pick1)
      val pick3 = next(pick2)
      val nextCurr = next(pick3)

      // Time to solve: 28 seconds
      /*
      val destination = Range(1, 10)
        .map(curr - _)
        .map(i => if (i <= 0) {
          cups.length + i
        } else {
          i
        })
        .filter(!Set(pick1, pick2, pick3).contains(_))
        .head
       */
      // Time to solve: 11 seconds
      var destination = curr - 1
      while (destination < 1 || Set(pick1, pick2, pick3).contains(destination)) {
        if (destination < 1) {
          destination = cups.length
        }
        destination -= 1
      }
      val afterDest = next(destination)

      next.put(curr, nextCurr)
      next.put(destination, pick1)
      next.put(pick3, afterDest)

      // println(LazyList.iterate(nextCurr)(c => next(c)).take(cups.length).mkString, curr, Vector(pick1, pick2, pick3), destination, nextCurr)

      curr = nextCurr
    }

    val p1 = next(1)
    val p2 = next(p1)
    println(p1, p2, p1.toLong * p2.toLong)
  }
}
