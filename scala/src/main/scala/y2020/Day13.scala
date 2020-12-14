package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day13 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day13")

    val startTime = input(0).toLong
    val buses = input(1).split(",").filter(x => x != "x").map(x => x.toInt).toVector

    {
      var found = false
      var time = startTime.toDouble
      while (!found) {
        for (b <- buses) {
          if ((time / b).isValidInt) {
            println("part1", b * (time.toInt - startTime), b, time.toInt - startTime)
            found = true
          }
        }
        time += 1
      }
    }


    {
      val buses2 = input(1).split(",").map{case "x" => "1"; case x => x}.map(x => x.toInt).toVector
      val busesWithIndex = buses2.zipWithIndex

      var t = BigInt(0)
      var step = BigInt(1)
      var searchRange = 1
      while(searchRange <= buses2.size) {
        val searchBuses = busesWithIndex.slice(0, searchRange)
        val found = searchBuses.forall(x => x._1 == 1 || (t + x._2).mod(x._1) == BigInt(0))
        if (found) {
          step = lcm(step, searchBuses.last._1)
          // actually the input is coprime so this also works! :hmm:
          // step = step * searchBuses.last._1
          searchRange += 1
          println("new step", step, searchRange)
        } else {
          t += step
        }

        println(t)
      }
    }
  }

  // stolen from rosetta code
  def gcd(a: BigInt, b: BigInt): BigInt = if (b==0) a.abs else gcd(b, a%b)
  def lcm(a: BigInt, b: BigInt): BigInt = (a*b).abs/gcd(a,b)
}
