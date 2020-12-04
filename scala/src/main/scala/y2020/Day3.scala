package y2020

object Day3 {
  def main(args: Array[String]): Unit = {
    val data = Util.readInput("day3")
    var valid = 0

    def isTree(y: Int, x: Int): Boolean = {
      val row = data(y)
      val cell = row(x % row.size)
      cell == '#'
    }

    // Part 1
    var (y, x) = (0, 0)
    var count = 0
    while (y < data.length) {
      if (isTree(y, x))  {
        count += 1
      }
      y = y + 1
      x = x + 3
    }
    println(count)

    // Part 2
    def move(ym: Int, xm: Int): Long = {
      var (y, x) = (0, 0)
      var count = 0
      while (y < data.length) {
        if (isTree(y, x))  {
          count += 1
        }
        y = y + ym
        x = x + xm
      }
      count
    }
    val r = move(1,1) * move(1, 3) * move(1, 5) * move(1, 7) * move(2, 1)
    println(r)
  }
}
