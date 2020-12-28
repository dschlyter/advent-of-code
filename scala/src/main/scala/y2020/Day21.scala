package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day21 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day21")

    Util.time("")

    part1(input)
    Util.time("part1")

    part2(input)
    Util.time("part2")
  }

  private def part1(input: Vector[String]) =  {
    val parsed = input.map(line => {
      val s = line.split("\\(contains ")
      (s(0).trim.split(" ").toSet, s(1).replace(")", "").split(",").map(_.trim).toSet)
    })

    val allFood = parsed.flatMap(_._1).toSet
    val allAllerg = parsed.flatMap(_._2).toSet

    var possibleSource = allAllerg.map(a => (a, Set[String]())).toMap

    parsed.foreach {
      case (food, allergens) =>
        allergens.foreach(a => {
          if (possibleSource(a).isEmpty) {
            possibleSource += (a -> food)
          } else {
            possibleSource += (a -> (possibleSource(a) & food))
          }
        })
    }
    val allPossible = possibleSource.values.flatten.toSet
    val noAllergens = allFood -- allPossible

    println(s"food without alergens ${noAllergens}")
    println(parsed.flatMap(_._1).count(noAllergens.contains))
  }

  private def part2(input: Vector[String]) =  {
    val parsed = input.map(line => {
      val s = line.split("\\(contains ")
      (s(0).trim.split(" ").toSet, s(1).replace(")", "").split(",").map(_.trim).toSet)
    })

    val allFood = parsed.flatMap(_._1).toSet
    val allAllerg = parsed.flatMap(_._2).toSet

    var possibleSource = allAllerg.map(a => (a, Set[String]())).toMap

    parsed.foreach {
      case (food, allergens) =>
        allergens.foreach(a => {
          if (possibleSource(a).isEmpty) {
            possibleSource += (a -> food)
          } else {
            possibleSource += (a -> (possibleSource(a) & food))
          }
        })
    }

    println(possibleSource)

    for (i <- Range(0,possibleSource.size)) {
      val uniqueFood = possibleSource.values.filter(_.size == 1).flatten.toSet
      possibleSource = possibleSource.mapValues(x => {
        if (x.size > 1) {
          x -- uniqueFood
        } else {
          x
        }
      }).toMap
    }

    println(possibleSource.toVector.sortBy(_._1).map(_._2.mkString("|||")).mkString(","))
  }
}
