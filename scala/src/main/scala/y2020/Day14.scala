package y2020
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day14 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day14")

    val parsed = input.map(line => {
      val p1 = line.split("=").map(_.trim).toVector
      val p2 = (p1(0).replace("]", "").split('[') ++ p1.tail).map(_.trim)
      p2.toVector
    })

    {
      var mask = "0" * 36
      val addr = mutable.Map[String, String]()

      for (inst <- parsed) {
        inst(0) match {
          case "mask" =>
            mask = inst(1)
          case _ =>
            addr(inst(1)) = applyMask(inst(2).toInt.toBinaryString, mask)
        }
      }

      var sum = 0L
      for (m <- addr.values) {
        sum += java.lang.Long.valueOf(m, 2).toLong
      }
      println(sum)
    }

    {
      var mask = "0" * 36
      val addr = mutable.Map[String, String]()

      for (inst <- parsed) {
        inst(0) match {
          case "mask" =>
            mask = inst(1)
          case _ =>
            val addrMask = applyMask2(inst(1).toInt.toBinaryString, mask)
            for (a <- expandMask(addrMask)) {
              addr(a) = inst(2)
            }
        }
      }

      var sum = 0L
      for (m <- addr.values) {
        sum += m.toLong
      }
      println(sum)
    }
  }

  def applyMask(value: String, mask: String): String = {
    val v = "0" * (mask.length - value.length) + value

    mask.toCharArray.zipWithIndex.map{
      case ('X', idx) => v.charAt(idx)
      case (c, _) => c
    }.mkString
  }

  def applyMask2(addr: String, mask: String): String = {
    val v = "0" * (mask.length - addr.length) + addr

    mask.toCharArray.zipWithIndex.map{
      case ('0', idx) => v.charAt(idx)
      case ('1', _) => '1'
      case ('X', _) => 'X'
    }.mkString
  }

  def expandMask(mask: String, prefix: String = ""): Vector[String] = {
    mask match {
      case "" => Vector(prefix)
      case x => x.head match {
        case 'X' =>
          expandMask(x.tail, prefix + '0') ++ expandMask(x.tail, prefix + '1')
        case c => expandMask(x.tail, prefix + c)
      }
    }
  }
}
