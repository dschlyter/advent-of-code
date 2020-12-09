package y2020
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Day8 {
  def main(args: Array[String]): Unit = {
    val input = Util.readInput("day8")

    val parsed = input.map(str => {
      str.split(" ", 2).toVector
    }).toVector

    val program = parsed

    // part 1
    {
      var executed = Set[Int]()
      var pc = 0
      var acc = 0
      while (!executed.contains(pc)) {
        executed += pc
        val inst = program(pc)(0)
        val arg = program(pc)(1)

        inst match {
          case "acc" => acc += Integer.parseInt(arg)
          case "jmp" => pc += Integer.parseInt(arg); pc -= 1
          case "nop" =>
        }

        pc += 1
      }
      println(acc)
    }


    // part 2
    for (i <- program.indices) {
      val modifiedInst =
        Vector(program(i)(0) match {
          case "nop" => "jmp"
          case "jmp" => "nop"
          case x => x
        }, program(i)(1))
      val modifiedProgram = program.updated(i, modifiedInst)

      var executed = Set[Int]()
      var pc = 0
      var acc = 0
      while (pc < program.size && !executed.contains(pc)) {
        executed += pc
        val inst = modifiedProgram(pc)(0)
        val arg = modifiedProgram(pc)(1)

        inst match {
          case "acc" => acc += Integer.parseInt(arg)
          case "jmp" => pc += Integer.parseInt(arg); pc -= 1
          case "nop" =>
        }

        pc += 1
      }

      if (!executed.contains(pc)) {
        println(acc)
      }
    }
  }
}
