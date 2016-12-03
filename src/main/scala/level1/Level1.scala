package level1

import scala.io.Source


/**
  * Created by olivierdeckers on 01/12/2016.
  */
case class Dir(id: Int) {
  def right: Dir = Dir((id + 1) % 4)
  def left: Dir = Dir((id + 3) % 4)

  def walk(pos: (Int,Int), steps: Int): (Int,Int) = {
    val (x,y) = pos
    id match {
      case 0 => (x, y+steps)
      case 1 => (x+steps, y)
      case 2 => (x, y-steps)
      case 3 => (x-steps, y)
    }
  }
}

object Level1 {

  val startPosition = ((0, 0), Dir(0))

  def calculateNextPosition(currentPos: ((Int, Int), Dir), instruction: String) = {
    val (pos, dir) = currentPos
    val newDir = instruction.charAt(0) match {
      case 'R' => dir.right
      case 'L' => dir.left
    }

    val steps = instruction.substring(1).toInt
    val newPos = newDir.walk(pos, steps)

    (newPos, newDir)
  }

  def calculateEndPosition(instructions: String) = {
    instructions.split(", ").foldLeft(startPosition)(calculateNextPosition)
  }

  def calculateFirstPositionVisitedTwice(instructions: String) = {
    val positions = instructions.split(", ").scanLeft(startPosition)(calculateNextPosition)
    val steps = positions.zip(positions.tail).flatMap {
      case ((pos1, _), (pos2, dir)) =>
        val steps = Seq.iterate(pos1, 200)(p => dir.walk(p, 1))
        steps.takeWhile(p => !p.equals(pos2))
    }
    steps.zipWithIndex.find {
      case (pos, idx) =>
        steps.drop(idx+1).find(x => x.equals(pos)).fold(false)(_ => true)
    }
  }

  def main(args: Array[String]): Unit = {
    val file = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("./level1/input"))
    val instructions = file.getLines().next()
//    val ((x,y),_) = calculateEndPosition(instructions)
//    println(x + y)
    println(calculateFirstPositionVisitedTwice(instructions))
  }

}
