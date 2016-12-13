import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by olivierdeckers on 11/12/2016.
  */
object Level8 {

  sealed trait Command
  case class Rect(width: Int, Height: Int) extends Command
  case class RotateRow(y: Int, amount: Int) extends Command
  case class RotateCol(x: Int, amount: Int) extends Command

  object Command {

    val rectRegex = new Regex("rect ([0-9]+)x([0-9]+)")
    val rotateRowRegex = new Regex("rotate row y=([0-9]+) by ([0-9]+)")
    val rotateColRegex = new Regex("rotate column x=([0-9]+) by ([0-9]+)")

    def parse(s: String): Command = {
      rectRegex.findFirstMatchIn(s).map(m => Rect(m.group(1).toInt, m.group(2).toInt))
        .orElse(rotateRowRegex.findFirstMatchIn(s).map(m => RotateRow(m.group(1).toInt, m.group(2).toInt)))
        .orElse(rotateColRegex.findFirstMatchIn(s).map(m => RotateCol(m.group(1).toInt, m.group(2).toInt)))
        .get
    }
  }

  val example =
    """rect 3x2
      |rotate column x=1 by 1
      |rotate row y=0 by 4
      |rotate column x=1 by 1""".stripMargin

  type Field = Array[Array[Int]]
//  val (width, height) = (7, 3)
  val (width, height) = (50, 6)
  var screen = Array.fill(height, width)(false)

  def processCommand(command: Command): Unit = command match {
    case Rect(w, h) =>
      for {
        x <- 0 until w
        y <- 0 until h
      } yield {
        screen(y)(x) = true
      }
    case RotateRow(y, amount) =>
      val originalRow = screen(y).clone()
      val newRow = 0.until(width).map { x =>
        originalRow((x - amount + width) % width)
      }.toArray
      screen(y) = newRow
    case RotateCol(x, amount) =>
      val transpose = screen.transpose
      val originalRow = transpose(x).clone()
      val newRow = 0.until(height).map { y =>
        originalRow((y - amount + height) % height)
      }.toArray
      transpose(x) = newRow
      screen = transpose.transpose
  }

  def showScreen(): Unit = {
    screen.foreach(row => println(row.map({case true => '#'; case false => '.'}).mkString("")))
  }

  def main(args: Array[String]): Unit = {
//    val lines = example.lines
    val lines = Source.fromURL(getClass.getResource("level8")).getLines()

    lines.foreach {line =>
      val command = Command.parse(line)
      processCommand(command)
    }
    println(screen.flatten.count(identity))
    showScreen()
  }


}
