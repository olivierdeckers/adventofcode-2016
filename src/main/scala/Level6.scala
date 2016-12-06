import scala.io.Source

/**
  * Created by olivierdeckers on 06/12/2016.
  */
object Level6 {

  val example = """eedadn
                  |drvtee
                  |eandsr
                  |raavrd
                  |atevrs
                  |tsrnev
                  |sdttsa
                  |rasrtv
                  |nssdts
                  |ntnada
                  |svetve
                  |tesnvt
                  |vntsnd
                  |vrdear
                  |dvrsen
                  |enarar""".stripMargin

  def main(args: Array[String]): Unit = {
//    val lines = example.lines
    val lines = Source.fromURL(getClass.getResource("level6")).getLines()
      .map(s => s.toCharArray).toArray
    val charCountsPerColumn = lines.transpose.map(line => line.groupBy(identity).mapValues(l => l.length).toSeq)
    println(charCountsPerColumn.map(_.sortBy(-_._2).head._1).mkString(""))
    println(charCountsPerColumn.map(_.sortBy(_._2).head._1).mkString(""))
  }
}
