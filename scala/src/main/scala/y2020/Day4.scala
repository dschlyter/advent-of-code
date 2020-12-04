package y2020
import scala.collection.{mutable => m}

object Day4 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day4")

    // part 1
    var passData = m.ArrayBuffer(m.Map[String, String]())
    input.foreach(line => {
      if (line == "") {
        passData.append(m.Map())
      } else {
        val s = line.split(" ")
        s.foreach(pair => {
          val s = pair.split(":")
          passData.last.put(s(0), s(1))
        })
      }
    })

    // part1
    val part1 = passData.map(pass => {
      Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").subsetOf(pass.keySet)
    }).count(identity)
    println(part1)

    // part2
    val part2 = passData.map(pass => {
      var valid = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").subsetOf(pass.keySet)
      try {
        valid = valid && pass("byr").length == 4 && pass("byr").toInt >= 1920 && pass("byr").toInt <= 2002
        valid = valid && pass("iyr").length == 4 && pass("iyr").toInt >= 2010 && pass("iyr").toInt <= 2020
        valid = valid && pass("eyr").length == 4 && pass("eyr").toInt >= 2020 && pass("eyr").toInt <= 2030
        val h = pass("hgt").substring(0, pass("hgt").length-2).toInt
        valid = valid && (pass("hgt").endsWith("cm") && h >= 150 && h <= 193 || pass("hgt").endsWith("in") && h >= 59 && h <= 76)
        valid = valid && pass("hcl").startsWith("#") && Integer.parseInt(pass("hcl").substring(1), 16) > -1
        valid = valid && Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(pass("ecl"))
        valid = valid && pass("pid").length == 9 && pass("pid").toInt > -1
        valid
      } catch {
        case e: Exception => {
          println(e)
          false
        }
      }
    }).count(identity)
    println(part2)

    // 108 too low
  }
}
