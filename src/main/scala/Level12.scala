import scala.io.Source

/**
  * Created by olivierdeckers on 15/12/2016.
  */
object Level12 {
  val example = """cpy 41 a
                  |inc a
                  |inc a
                  |dec a
                  |jnz a 2
                  |dec a""".stripMargin

  def main(args: Array[String]): Unit = {
//    val lines = example.lines.toList
    val lines = Source.fromURL(getClass.getResource("level12")).getLines().toList
    var state = Map('a' -> 0, 'b' -> 0, 'c' -> 0, 'd' -> 0)
//    var state2 = Map('a' -> 0, 'b' -> 0, 'c' -> 1, 'd' -> 0)

    var curLine = 0
    while (curLine < lines.length) {
      val line = lines(curLine)
      val instruction = line.take(3)
      val parts = line.drop(4).split(" ")
      state = updateState(state, instruction, parts)


      if (instruction == "jnz") {
        val amount = try {
          parts.head.toInt
        } catch {
          case _: NumberFormatException => state(parts.head.head)
        }
        if (amount != 0) {
          val amount = parts(1).toInt
          curLine += amount
        } else {
          curLine += 1
        }
      } else {
        curLine += 1
      }
    }

    println(state)
  }

  def updateState(state: Map[Char, Int], instruction: String, parts: Seq[String]): Map[Char, Int] = instruction match {
    case "cpy" =>
      val amount = try {
        parts.head.toInt
      } catch {
        case _: NumberFormatException =>
          val reg = parts.head.head
          state(reg)
      }
      val reg = parts(1).head
      state.updated(reg, amount)
    case "inc" =>
      val reg = parts.head.head
      state.updated(reg, state(reg)+1)
    case "dec" =>
      val reg = parts.head.head
      state.updated(reg, state(reg)-1)
    case "jnz" =>
      state
  }
}
