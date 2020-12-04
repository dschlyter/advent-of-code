package y2020

object Day2 {
  def main(args: Array[String]): Unit = {
    val data = Util.readInput("day2")
    var valid = 0

    // Part 1
    data.foreach(d => {
      val s = d.split("[- :]").toList
      val (start, stop, letter, password) = (s(0).toInt, s(1).toInt, s(2).charAt(0), s(4))
      val count = password.count(_ == letter)
      if (start <= count && count <= stop) {
        valid += 1
      }
    })
    println(valid)

    valid = 0

    // Part 2
    data.foreach(d => {
      val s = d.split("[- :]").toList
      val (start, stop, letter, password) = (s(0).toInt, s(1).toInt, s(2).charAt(0), s(4))

      val firstMatch = password.charAt(start-1) == letter
      val secondMatch = password.charAt(stop-1) == letter
      if ((firstMatch && !secondMatch) || (!firstMatch && secondMatch)) {
        valid += 1
      }
    })
    println(valid)
  }
}
