package level2

import scala.io.Source
import math.{max, min}

/**
 * Created by olivierdeckers on 02/12/2016.
 */
object Level2 {

  def clamp(x: Int, y: Int, size: Int = 2): (Int, Int) = {
    val clampedX = max(min(x, size), 0)
    val clampedY = max(min(y, size), 0)
    (clampedX, clampedY)
  }

  def main(args: Array[String]): Unit = {
    val solution: Iterator[Int] = partOne
    println(solution.toArray.mkString(""))
    println(partTwo.toArray.toSeq)
  }

  def partTwo = {
    val table = Seq(
      Seq(0,  0,   1,   0,  0),
      Seq(0,  2,   3,   4,  0),
      Seq(5,  6,   7,   8,  9),
      Seq(0, 'A', 'B', 'C', 0),
      Seq(0,  0,  'D',  0,  0)
    )
    val solution = instructions.map { instruction =>
      val initialPosition = (2, 0)
      val endPos = instruction.foldLeft(initialPosition) { case ((row,col), dir) =>
        val newPos = dir match {
          case 'U' => (row-1, col)
          case 'D' => (row+1, col)
          case 'L' => (row, col-1)
          case 'R' => (row, col+1)
        }
        val clampedNewPos = clamp(newPos._1, newPos._2, 4)

        if (table(clampedNewPos._1)(clampedNewPos._2) != 0) {
          clampedNewPos
        } else {
          (row,col)
        }
      }
      table(endPos._1)(endPos._2)
    }
    solution
  }

  private def partOne = {
    val table = Seq.tabulate(3, 3)((x, y) => y * 3 + x + 1)
    val solution = instructions.map { instruction =>
      val initialPosition = (1, 1)
      val endPos = instruction.foldLeft(initialPosition) {
        case ((x, y), 'U') => clamp(x, y - 1)
        case ((x, y), 'D') => clamp(x, y + 1)
        case ((x, y), 'L') => clamp(x - 1, y)
        case ((x, y), 'R') => clamp(x + 1, y)
      }
      table(endPos._1)(endPos._2)
    }
    solution
  }

  private def instructions = {
    val input = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("./level2/input"))
    input.getLines()
  }
}
