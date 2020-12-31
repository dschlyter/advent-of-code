package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day22 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day22")

    Util.time("")

    part1(input)
    Util.time("part1")

    part2(input)
    Util.time("part2")
  }

  private def part1(input: Vector[String]) =  {
    val grouped = Util.groupInput(input).map(_.drop(1)).map(_.map(_.toInt))
    var p1 = grouped(0)
    var p2 = grouped(1)

    println(grouped)

    var round = 1
    while(p1.nonEmpty && p2.nonEmpty) {
      round += 1
      println(round)

      val c1 = p1.head
      val c2 = p2.head
      p1 = p1.drop(1)
      p2 = p2.drop(1)

      if (c1 > c2) {
        p1 = p1 ++ Vector(c1, c2)
      } else {
        p2 = p2 ++ Vector(c2, c1)
      }
    }

    if (p1.nonEmpty) {
      println(p1)
      score(p1)
    } else {
      println(p2)
    }
  }

  private def score(p1: Vector[Int]) =  {
    println(p1.zipWithIndex.map{case (card, index) => card * (p1.size - index)}.sum)
  }

  private def part2(input: Vector[String]) =  {
    val grouped = Util.groupInput(input).map(_.drop(1)).map(_.map(_.toInt))
    var p1 = grouped(0)
    var p2 = grouped(1)

    subGameMem(p1, p2)
  }

  val mem = mutable.Map[(Vector[Int], Vector[Int]), Boolean]()

  private def subGameMem(p1s: Vector[Int], p2s: Vector[Int]): Boolean = {
    val key = (p1s, p2s)
    if (mem.contains(key)) {
      return mem(key)
    }
    val res = subGame(p1s, p2s)
    mem.put(key, res)
    res
  }

  private def subGame(p1s: Vector[Int], p2s: Vector[Int]): Boolean = {
    var p1 = p1s
    var p2 = p2s

    val played = mutable.Set[(Vector[Int], Vector[Int])]()

    while(p1.nonEmpty && p2.nonEmpty) {
      if (played.contains((p1, p2))) {
        return true
      }
      played.add((p1, p2))

      val c1 = p1.head
      val c2 = p2.head
      p1 = p1.drop(1)
      p2 = p2.drop(1)

      val p1Wins = if (c1 <= p1.length && c2 <= p2.length) {
        subGameMem(p1.slice(0, c1), p2.slice(0, c2))
      } else {
        c1 > c2
      }

      if (p1Wins) {
        p1 = p1 ++ Vector(c1, c2)
      } else {
        p2 = p2 ++ Vector(c2, c1)
      }
    }

    val p1Wins = p1.nonEmpty
    if (p1Wins) {
      score(p1)
    } else {
      score(p2)
    }

    return p1Wins
  }
}
