import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by olivierdeckers on 07/12/2016.
  */
object Level7 {
  val example =
    """abba[mnop]qrst
      |abcd[bddb]xyyx
      |aaaa[qwer]tyui
      |ioxxoj[asdfgh]zxcvbn
      |xabb[mnop]aqrs""".stripMargin

  val example2 =
    """aba[bab]xyz
      |xyx[xyx]xyx
      |aaa[kek]eke
      |zazbz[bzb]cdb""".stripMargin

  def main(args: Array[String]): Unit = {
//    val input = example2.lines
    val input = Source.fromURL(getClass.getResource("level7")).getLines()
    val regex = new Regex("""\[[a-z]+\]""")
    val parts = input.map { line =>
      val inBrackets = regex.findAllIn(line).map(_.replace("[", "").replace("]", "")).toSeq
      val outBrackets = regex.replaceAllIn(line, ",").split(",")
      (inBrackets, outBrackets)
    }

//    val result1 = parts.filter { case (in, out) =>
//      out.exists(containsAbba) && in.forall(!containsAbba(_))
//    }
//    println(result1.size)

    val result2 = parts.filter { case (in, out) =>
      val abas = out.flatMap(getAbas)
      abas.exists({ case (a, b) => in.exists(s => containsBab(s, a, b)) })
    }

    println(result2.size)
  }

  def getAbas(s: String): List[(Char, Char)] = getAbas(s.toCharArray.toList)
  def getAbas(s: List[Char]): List[(Char, Char)] = s match {
    case (s1 :: s2 :: s3 :: rest) =>
      if (s1 == s3 && s1 != s2) {
        (s1, s2) :: getAbas(s.tail)
      } else {
        getAbas(s.tail)
      }
    case _ => List()
  }

  def containsBab(s: String, a: Char, b: Char): Boolean = {
    val regex = new Regex(s"$b$a$b")
    regex.findFirstMatchIn(s).isDefined
  }

  def containsAbba(s: String): Boolean = containsAbba(s.toCharArray.toList)
  def containsAbba(s: List[Char]): Boolean = s match {
    case (s1 :: s2 :: s3 :: s4 :: rest) =>
      (s1 != s2 && s1 == s4 && s2 == s3) || containsAbba(s.tail)
    case _ => false
  }
}
