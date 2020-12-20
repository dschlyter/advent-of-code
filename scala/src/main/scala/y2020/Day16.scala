package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day16 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day16")
    Util.time("")

    part1(input)
    Util.time("part1")

    part2(input)
    Util.time("part2")

    // 40.358742019 with mem
    // 40.80228167 without mem :hmm:
  }

  private def part1(input: Vector[String]) =  {
    val rules = input.takeWhile(s => s != "")
    val yourTicket = input(rules.length+2).split(",").map(_.toInt).toVector
    val nearbyTickets = input.drop(rules.length+5).map(_.split(",").map(_.toInt).toVector)

    val parsedRules = rules.map(line => {
      val s = line.split(":")
      (s(0).trim, s(1).split("or").map(s => s.trim.split("-").map(_.toInt).toVector).toVector)
    })

    val invalidNumbers = nearbyTickets.flatten.filter(!validNumber(parsedRules, _))
    println(invalidNumbers.sum)
  }

  private def part2(input: Vector[String]) =  {
    val rules = input.takeWhile(s => s != "")
    val yourTicket = input(rules.length+2).split(",").map(_.toInt).toVector
    val nearbyTickets = input.drop(rules.length+5).map(_.split(",").map(_.toInt).toVector)

    val parsedRules = rules.map(line => {
      val s = line.split(":")
      (s(0).trim, s(1).split("or").map(s => s.trim.split("-").map(_.toInt).toVector).toVector)
    })

    val validTickets = nearbyTickets.filter(ticket => {
      ticket.forall(validNumber(parsedRules, _))
    }) :+ yourTicket

    val valid = mutable.Map[Int, Vector[Int]]()

    for (i <- rules.indices) {
      val rule = parsedRules(i)
      for (j <- yourTicket.indices) {
        if (validTickets.forall(t => validNumber(rule, t(j)))) {
          valid(i) = valid.getOrElse(i, Vector()) :+ j
        }
      }
    }

    println(valid)
    val solved = matchConstraintsMem(valid.toMap, Set(), valid.keys.toVector)
    println(solved)
    println(solved.get.map(x => (parsedRules(x._1)._1, x._2)).toMap)

    val mappedTicket = solved.get.map(x => (parsedRules(x._1)._1, yourTicket(x._2)))
    println("mappedTicket", mappedTicket)
    println(mappedTicket.filter(_._1.contains("departure")))
    println(mappedTicket.filter(_._1.contains("departure")).values.map(_.toLong).product)
  }

  private def validNumber(parsedRules: Vector[(String, Vector[Vector[Int]])], num: Int): Boolean =  {
    parsedRules.exists(rule => validNumber(rule, num))
  }

  private def validNumber(rule: (String, Vector[Vector[Int]]), num: Int): Boolean =  {
    rule._2.exists(range => range(0) <= num && num <= range(1))
  }

  private def matchConstraints(valid: Map[Int, Vector[Int]], used: Set[Int], fieldsLeft: Vector[Int]): Option[Map[Int, Int]] = {
    if (fieldsLeft.nonEmpty) {
      val field = fieldsLeft.head
      for (position <- valid(field) if !used.contains(position)) {
        matchConstraints(valid, used + position, fieldsLeft.tail) match {
          case Some(solution) => return Some(solution + (field -> position))
          case None =>
        }
      }
      None
    } else {
      Some(Map())
    }
  }

  val mem = mutable.Map[(Set[Int], Vector[Int]), Option[Map[Int, Int]]]()

  private def matchConstraintsMem(valid: Map[Int, Vector[Int]], used: Set[Int], left: Vector[Int]): Option[Map[Int, Int]] = {
    val key = (used, left)
    if (mem.contains(key)) {
      return mem(key)
    }

    val ret = matchConstraints(valid, used, left)
    mem(key) = ret
    ret
  }
}
