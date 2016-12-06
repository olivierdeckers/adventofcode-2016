import scala.collection.immutable.NumericRange.Inclusive
import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by olivierdeckers on 04/12/2016.
  */
object Level4 {

  val examples =
    """aaaaa-bbb-z-y-x-123[abxyz]
      |a-b-c-d-e-f-g-h-987[abcde]
      |not-a-real-room-404[oarel]
      |totally-real-room-200[decoy]""".stripMargin

  def main(args: Array[String]): Unit = {
//    val lines = examples.lines
    val lines = Source.fromURL(getClass.getResource("level4")).getLines.toSeq
    val sequenceWithValidity = extractValidSequences(lines)
    println(sequenceWithValidity.filter(_._2).map(_._1).sum)

    val decryptedNames = sequenceWithValidity.map({
      case (seq, _, letters) => (seq, rotateLetters(letters, seq))
    })
    println(decryptedNames.filter(s => s._2.contains("northpole")).toList)
  }

  val alphabet: Inclusive[Char] = 'a'.to('z')
  def rotateLetters(letters: String, nbTimes: Int) = {
    letters.map { c =>
      val idx = (alphabet.indexOf(c) + nbTimes) % alphabet.length
      alphabet(idx)
    }
  }

  private def extractValidSequences(lines: Seq[String]) = {
    val regex = new Regex("""([a-z\-]+)-([0-9]+)\[([a-z]+)\]""")
    val sequenceWithValidity = lines.map { line =>
      val Some(rmatch) = regex.findFirstMatchIn(line)
      val letters = rmatch.group(1).replace("-", "")
      val sequence = rmatch.group(2).toInt
      val checksum = rmatch.group(3)
      val letterCounts = letters.groupBy(identity).mapValues(_.length)
      val test = letterCounts.toSeq.sortBy({ case (char, count) => (-count, char) }).map(_._1).mkString.take(5)
      (sequence, test == checksum, letters)
    }
    sequenceWithValidity
  }
}
