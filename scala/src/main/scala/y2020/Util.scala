package y2020

object Util {
  def hello = "hello"

  def readInput(inputName: String) = {
    val source = io.Source.fromFile(s"src/main/scala/y2020/input/$inputName.txt")
    try {
      source.getLines.toList
    } finally {
      source.close()
    }
  }
}
