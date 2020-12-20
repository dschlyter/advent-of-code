package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day19 {
  def main(args: Array[String]): Unit = {
    val Vector(rules, input) = Util.groupedInput("day19")
    val parsedRules =
      rules
        .sortBy(s => s.split(":")(0).toInt)
        .map(line => line.dropWhile(_ != ' ').trim)
        .map(line => line.split(" [|] ").toVector.map(rule =>
          if (rule.startsWith("\"")) {
            Left(rule.charAt(1))
          } else {
            Right(rule.split(" ").map(_.toInt).toVector)
          }
        )
    )
    println(parsedRules)

    Util.time("")

    // Note: We exploit the fact that all rule lists options A | B have the same length
    {
      var count = 0
      for (i <- input) {
        if (consumeWithRule(i, 0, parsedRules).contains("")) {
          count += 1
        }
      }
      println(count)
    }

    Util.time("part1")

    // DOH! Our assumption failed for part2 :(
    // Solve part1 again with new method, expect same result
    {
      var count = 0
      for (i <- input) {
        if (advanceWithRule(parsedRules, i, 0, Set(0)).contains(i.length)) {
          count += 1
        }
      }
      println(count)
    }

    {
      var rules2 = parsedRules
      rules2 = rules2.updated(8, Vector(Right(Vector(42)), Right(Vector(42, 8))))
      rules2 = rules2.updated(11, Vector(Right(Vector(42, 31)), Right(Vector(42, 11, 31))))

      var count = 0
      for (i <- input) {
        if (advanceWithRule(rules2, i, 0, Set(0)).contains(i.length)) {
          count += 1
        }
      }
      println(count)
    }
    Util.time("part2")
  }

  def consumeWithRule(s: String, ruleIndex: Int, rules: Vector[Vector[Either[Char, Vector[Int]]]]): Option[String] = {
    val ruleAlts = rules(ruleIndex)

    for (rule <- ruleAlts) {
      val matching = rule match {
        case Left(c) =>
          if (s.charAt(0) == c) {
            Some(s.substring(1))
          } else {
            None
          }

        case Right(ruleList) =>
          ruleList.foldLeft(Option(s))((matchingStatus, subRuleIndex) => {
            matchingStatus match {
              case Some(unmatched) => consumeWithRule(unmatched, subRuleIndex, rules)
              case None => None
            }
          })
      }

      if (matching.isDefined) {
        return matching
      }
    }

    None
  }

  // Note: We exploit the fact that all rule lists options A | B have the same length
  def advanceWithRule(rules: Vector[Vector[Either[Char, Vector[Int]]]], s: String, ruleIndex: Int, possiblePositions: Set[Int]): Set[Int] = {
    val ruleAlts = rules(ruleIndex)

    val newPos = for (rule <- ruleAlts) yield {
      rule match {
        case Left(c) => possiblePositions.filter(i => s.length > i && s.charAt(i) == c).map(_ + 1)
        case Right(ruleList) => ruleList.foldLeft(possiblePositions)(
          (newPositions, subRuleIndex) => newPositions match {
            case s if s.isEmpty => s
            case _ => advanceWithRule(rules, s, subRuleIndex, newPositions)
          }
        )
      }
    }

    newPos.reduce((a,b) => a ++ b)
  }
}
