package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day24 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day24")

    Util.time("")

    part1(input)
    Util.time("part1")

    part2(input)
    Util.time("part2")
  }

  // Shared between part 1 and part2
  var tileColor = Map[(Int, Int), Boolean]()

  private def part1(input: Vector[String]) =  {
    input.foreach(line => {
      var i = 0
      var y = 0
      var x = 0

      while (i < line.length) {
        val fc = line.charAt(i)
        fc match {
          case 'e' => x += 1
          case 'w' => x -= 1
          case 'n' | 's' =>
            val base = if (y % 2 == 0) x else x-1
            x = base + (if (line.charAt(i+1) == 'e') 1 else 0)
            y = y + (if (fc == 'n') 1 else -1)
            i += 1
        }
        i += 1
      }

      val pos = (y,x)
      tileColor = tileColor + (pos -> !tileColor.getOrElse(pos, false))
    })

    println(tileColor.values.count(x => x))
    println(tileColor.filter(x => x._2))
  }

  private def part2(input: Vector[String]) =  {
    for (day <- Range(0, 100)) {
      tileColor = tileColor
        .filter(_._2)
        .keys
        .tapEach(x => println("pos", x))
        .toSet[(Int, Int)]
        .flatMap(pos => neigh(pos) ++ Set(pos))
        .map(pos => {
          val adjBlack = neigh(pos).count(np => tileColor.getOrElse(np, false))
          // val adjBlack = neigh(pos).map(np => tileColor.getOrElse(np, false)).count(x => x)

          if (tileColor.getOrElse(pos, false)) {
            (pos, !(adjBlack == 0 || adjBlack > 2))
          } else {
            (pos, adjBlack == 2)
          }
        })
        .toMap

      // println(day, tileColor.values.count(x => x))
    }

    println(tileColor.values.count(x => x))
  }

  private def neigh(pos: (Int, Int)): Set[(Int, Int)] = {
    pos match {
      case (y: Int, x: Int) =>
        val base = if (y % 2 == 0) x else x-1

        Set(
          (y,x-1),
          (y,x+1),
          (y-1,base),
          (y-1,base+1),
          (y+1,base),
          (y+1,base+1),
        )
    }
  }
}
