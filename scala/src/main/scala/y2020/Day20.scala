package y2020
import scala.collection.mutable

object Day20 {
  def main(args: Array[String]): Unit = {
    val input = Util.groupedInput("day20")

    Util.time("")

    part1(input)
    Util.time("part1")

    // Lazy way to flip stuff
    Range(0,4).foreach(start=> part2(input, start))
    Util.time("part2")
  }

  case class Tile(id: Int, tile: Vector[String], edges: Vector[String], rot: Int = 0, flip: Boolean = false) {
    def print(): Unit = {
      println(s"Tile: ${id} ${rot} ${flip}")
      tile.foreach(println)
    }
  }

  private def part1(input: Vector[Vector[String]]) =  {
    val tiles = input.map(v => (v.head.replace("Tile ", "").replace(":","").toInt, v.tail))

    val withEdgeKeys = tiles.map {
      case (title, tile) =>
        val top = tile.head
        val right = tile.map(s => s.substring(s.length-1, s.length)).reduce((a,b) => a+b)
        val bottom = tile.last.reverse
        val left = tile.map(s => s.substring(0, 1)).reduce((a,b) => a+b).reverse

        Tile(title, tile, Vector(top, right, bottom, left))
    }

    val unmatchedEdges = withEdgeKeys.flatMap(x => {
      x.edges.flatMap(s => List(s, s.reverse))
    }).groupBy(x => x).view.mapValues(list => list.size).filter(x => x._2 <= 1).keySet

    val prod = withEdgeKeys
      .filter(x => x.edges.count(edge => !unmatchedEdges.contains(edge)) == 2)
      .map(_.id.toLong)
      .product

    println(prod)
  }

  private def part2(input: Vector[Vector[String]], startCorner: Int) =  {
    val withTitle = input.map(v => (v.head.replace("Tile ", "").replace(":","").toInt, v.tail))

    val tiles = withTitle.map {
      case (title, tile) =>
        Tile(title, tile, findEdges(tile))
    }

    val tileLookup = tiles.flatMap(tile => {
      tile.edges
        .flatMap(s => List(s, s.reverse))
        .map(edge => (edge,tile))
    }).groupMap(_._1)(_._2)

    val corner = tiles.filter(_.edges.count(x => tileLookup(x).size == 1) == 2)(startCorner)
    val topLeftOriented = allOrientations(corner).filter(tile =>
      tileLookup(tile.edges(0)).size == 1
        && tileLookup(tile.edges(3)).size == 1).head

    // Assume square
    val sideLength = math.sqrt(tiles.size).toInt

    var grid = Vector(Vector(topLeftOriented))

    for (y <- Range(0, sideLength); x <- Range(0, sideLength)) {
      if (y == 0 && x == 0) {
        // Skip
      } else if (y == 0) {
        val last = grid(y)(x-1)
        val next = tileLookup(last.edges(1)).filter(_.id != last.id).head
        val validFlips = allOrientations(next).filter(_.edges(3) == last.edges(1))
        assert(validFlips.size == 1)
        grid = grid.updated(0, grid(0) :+ validFlips.head)
      } else {
        if (x == 0) {
          grid = grid :+ Vector()
        }
        val last = grid(y-1)(x)
        val next = tileLookup(last.edges(2)).filter(_.id != last.id).head
        val validFlips = allOrientations(next).filter(_.edges(0) == last.edges(2))
        assert(validFlips.size == 1)
        grid = grid.updated(y, grid(y) :+ validFlips.head)
      }
    }

    val withoutEdges = grid.map(gridRow => gridRow.map(tile => tile.tile.drop(1).dropRight(1).map(row => row.substring(1, row.length - 1))))
    val oneGrid = withoutEdges.flatMap(gridRow => gridRow.transpose.map(_.mkString))

    val monster = Vector(
      "                  # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   "
    )

    var monsterCount = 0
    for (startY <- oneGrid.indices;
         startX <- oneGrid(startY).indices) {
      if(
        (for (y <- monster.indices; x <- monster(y).indices) yield (y,x)).forall {
          case (y, x) => monster(y).charAt(x) == ' ' || oneGrid.lift(startY + y).exists(row => row.lift(startX + x).contains('#'))
        }
      ) {
        monsterCount += 1
      }
    }

    println(s"there are ${monsterCount} monsters")
    val sea = oneGrid.mkString.count(_ == '#') - monsterCount * monster.mkString.count(_ == '#')
    println(sea)
  }

  private def allOrientations(tile: Tile): Seq[Tile] = {
    val withFlip = List(tile, mirror(tile))
    withFlip
      .flatMap(t => {
        val r90 = rot90Clockwise(t)
        val r180 = rot90Clockwise(r90)
        val r270 = rot90Clockwise(r180)
        List(t, r90, r180, r270)
      })
  }

  private def mirror(tile: Tile): Tile = {
    val newTile = tile.tile.map(row => row.reverse)
    Tile(tile.id, newTile, findEdges(newTile), tile.rot, !tile.flip)
  }

  private def rot90Clockwise(tile: Tile): Tile = {
    val newTile = tile.tile(0).indices.toVector.map(i => tile.tile.map(row => row.charAt(i)).reverse.mkString)
    Tile(tile.id, newTile, findEdges(newTile), (tile.rot + 90) % 360, tile.flip)
  }

  private def findEdges(tile: Vector[String]): Vector[String] =  {
    val top = tile.head
    val right = tile.map(s => s.substring(s.length-1, s.length)).mkString
    val bottom = tile.last
    val left = tile.map(s => s.substring(0, 1)).mkString

    Vector(top, right, bottom, left)
  }
}
