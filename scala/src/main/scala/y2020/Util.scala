package y2020
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Util {
  def hello = "hello"

  def readInput(inputName: String): List[String] = {
    val source = io.Source.fromFile(s"src/main/scala/y2020/input/$inputName.txt")
    try {
      source.getLines.toList
    } finally {
      source.close()
    }
  }

  def groupedInput(inputName: String, delimiter: String = ""): Vector[Vector[String]] = {
    val input = readInput(inputName)

    input.foldLeft(Vector(Vector[String]()))((agg, a) => {
      if (a == delimiter) {
        agg ++ Vector(Vector[String]())
      } else {
        agg.dropRight(1) :+ (agg.last :+ a)
      }
    })
  }

  var lastTime = 0L

  def time(desc: String): Unit = {
    if (lastTime > 0) {
      println("TIME "+desc+": "+((System.nanoTime() - lastTime) / 1e9))
    }
    lastTime = System.nanoTime()
  }
}
