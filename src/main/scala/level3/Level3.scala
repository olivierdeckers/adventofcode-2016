package level3

import scala.io.Source

/**
  * Created by olivierdeckers on 03/12/2016.
  */
object Level3 {

  def main(args: Array[String]): Unit = {
    val triangles = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("./level3/input")).getLines()
    val lengths = triangles.map(s => {
      s.trim.split("\\ +").map(_.toInt)
    }).toSeq
    println(validTriangles(lengths).length)

    val lengthTuples = lengths.map {
      case Array(a, b, c) => (a, b, c)
    }
    val (col1, col2, col3) = lengthTuples.toList.unzip3
    val newTriangles = (col1 ++ col2 ++ col3).grouped(3).map(_.toArray).toSeq
    println(validTriangles(newTriangles).length)
  }

  private def validTriangles(lengths: Seq[Array[Int]]) = {
    lengths.filter { list =>
      list.permutations.forall {
        case Array(a, b, c) => a + b > c
      }
    }
  }
}
