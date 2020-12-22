package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

// For tuple comparisons
// https://stackoverflow.com/questions/11102393/how-to-lexicographically-compare-scala-tuples
import scala.math.Ordering.Implicits._

object Day18 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day18")
    println(input)

    Util.time("")

    part1(input)
    Util.time("part1")

    part2(input)
    Util.time("part2")
  }

  private def part1(input: Vector[String]) =  {
    var sum = BigInt(0)

    for (line <- input) {
      val line2 = line.replace("(", " ( ").replace(")", " ) ").replaceAll(" +", " ").trim

      val output = mutable.Stack[BigInt]()
      val operators = mutable.Stack[(Int, String)]()
      var nestingLevel = 0

      def applyOperators(nesting: Int): Unit = {
        while (operators.nonEmpty && operators.head._1 >= nesting) {
          val op = operators.pop()._2
          val arg2 = output.pop()
          val arg1 = output.pop()
          val res = op match {
            case "+" => arg1 + arg2
            case "*" => arg1 * arg2
          }
          output.push(res)
          // println("pop =>", output, operators)
        }
      }

      line2.split(" ").foreach {
        case token @ ("+" | "*") =>
          applyOperators(nestingLevel)
          operators.push((nestingLevel, token))
        case "(" =>
          nestingLevel += 1
        case ")" =>
          nestingLevel -= 1
        case token =>
          output.push(token.toLong)
      }
      applyOperators(0)

      // println("ans", output.head)
      sum += output.head
    }
    println("part1", sum)
  }

  private def part2(input: Vector[String]) =  {
    var sum = BigInt(0)

    // Full shunting yard
    for (line <- input) {
      val line2 = line.replace("(", " ( ").replace(")", " ) ").replaceAll(" +", " ").trim

      val output = mutable.Stack[BigInt]()
      val operators = mutable.Stack[((Int, Int), String)]()
      var nestingLevel = 0

      def applyOperators(nesting: Int, nextOperatorPrio: Int): Unit = {
        while (operators.nonEmpty && operators.head._1 >= (nesting, nextOperatorPrio)) {
          val op = operators.pop()._2
          val arg2 = output.pop()
          val arg1 = output.pop()
          val res = op match {
            case "+" => arg1 + arg2
            case "*" => arg1 * arg2
          }
          output.push(res)
          // println("pop =>", output, operators)
        }
      }

      line2.split(" ").foreach {
        case token @ ("+" | "*") =>
          val prio = token match {
            case "+" => 1
            case "*" => 0
          }
          applyOperators(nestingLevel, prio)
          operators.push(((nestingLevel, prio), token))
        case "(" =>
          nestingLevel += 1
        case ")" =>
          nestingLevel -= 1
        case token =>
          output.push(token.toLong)
      }
      applyOperators(0, 0)

      println("ans", output.head)
      sum += output.head
    }
    println("part2", sum)
  }
}
