package y2020
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Util {
  def hello = "hello"

  def readInput(inputName: String): Vector[String] = {
    val source = io.Source.fromFile(s"src/main/scala/y2020/input/$inputName.txt")
    try {
      source.getLines.toVector
    } finally {
      source.close()
    }
  }

  def groupedInput(inputName: String, delimiter: String = ""): Vector[Vector[String]] = {
    groupInput(readInput(inputName), delimiter)
  }

  def groupInput(input: Vector[String], delimiter: String = "") =  {
    val grouped = input.foldLeft(Vector(Vector[String]()))((agg, a) => {
      if (a == delimiter) {
        agg ++ Vector(Vector[String]())
      } else {
        agg.dropRight(1) :+ (agg.last :+ a)
      }
    })

    grouped.last match {
      case Vector() => grouped.dropRight(1)
      case _ => grouped
    }
  }

  var lastTime = 0L

  def time(desc: String): Unit = {
    if (lastTime > 0) {
      println("TIME "+desc+": "+((System.nanoTime() - lastTime) / 1e9))
    }
    lastTime = System.nanoTime()
  }
}
