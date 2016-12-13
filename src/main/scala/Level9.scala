import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by olivierdeckers on 13/12/2016.
  */
object Level9 {
  val example =
    """ADVENT
      |A(1x5)BC
      |(3x3)XYZ
      |A(2x2)BCD(2x2)EFG
      |(6x1)(1x3)A
      |X(8x2)(3x3)ABCY""".stripMargin

  val example2 =
    """(3x3)XYZ
      |X(8x2)(3x3)ABCY
      |(27x12)(20x12)(13x14)(7x10)(1x12)A
      |(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN""".stripMargin

  val markerRegex = new Regex("""^\((\d+)x(\d+)\)""")
  def expandLine(s: String): String = expandLine(s.toCharArray)
  def expandLine(s: Array[Char]): String = s match {
    case Array() => ""
    case _ =>
      markerRegex.findPrefixMatchOf(s).map { m =>
        val (width, times) = (m.group(1).toInt, m.group(2).toInt)
        val rest = s.drop(m.group(0).length)
        val sequence = rest.take(width)
        val expandedSequence = Array.fill(times)(sequence).flatten
        (expandedSequence ++ expandLine(rest.drop(width))).mkString
      }.getOrElse(s.head + expandLine(s.tail))
  }

  def expandedLength(s: String): BigInt = expandedLength(s.toCharArray)
  def expandedLength(s: Array[Char]): BigInt = s match {
    case Array() => 0
    case _ =>
      markerRegex.findPrefixMatchOf(s).map { m =>
        val (width, times) = (m.group(1).toInt, m.group(2).toInt)
        val rest = s.drop(m.group(0).length)
        val sequenceLength = expandedLength(rest.take(width))
        times * sequenceLength + expandedLength(rest.drop(width))
      }.getOrElse(1 + expandedLength(s.tail))
  }

  def main(args: Array[String]): Unit = {
    val lines = example2.lines
    lines.map(expandLine).foreach(s => println(s.length, s))

    val input = Source.fromURL(getClass.getResource("level9")).getLines().next().replace(" ", "")
    val length = expandedLength(input)
    println(length)
  }
}
