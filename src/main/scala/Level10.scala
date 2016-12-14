import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by olivierdeckers on 14/12/2016.
  */
object Level10 {

  val example =
    """value 5 goes to bot 2
      |bot 2 gives low to bot 1 and high to bot 0
      |value 3 goes to bot 1
      |bot 1 gives low to output 1 and high to bot 0
      |bot 0 gives low to output 2 and high to output 0
      |value 2 goes to bot 2""".stripMargin

  val inRegex = new Regex("""value (\d+) goes to bot (\d+)""")
  val logicRegex = new Regex("""bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""")

  sealed trait Dest
  case class BotDest(i: Int) extends Dest
  case class BinDest(i: Int) extends Dest

  case class Bot(vals: Seq[Int], lowDest: Option[Dest], highDest: Option[Dest])


  def main(args: Array[String]): Unit = {
  val initialBots = Map[Int, Bot]()
  val initialBins = Map[Int, Int]()
  val initialState = (initialBots, initialBins)

//    val lines = example.lines
    val lines = Source.fromURL(getClass.getResource("level10")).getLines()

    val (_, bins) = lines.foldLeft(initialState) { case ((bots, bins), line) =>
      val (botId: Int, newBot: Bot) = updateBotFromLine(bots, line)
      val (bots2, bins2) = (bots.updated(botId, newBot), bins)
      triggerTransfer(bots2, bins2, botId)
    }

    println((0 to 2).map(bins).product)
  }

  private def updateBotFromLine(bots: Map[Int, Bot], line: String): (Int, Bot) = {
    val inMatch = inRegex.findFirstMatchIn(line)
    val logicMatch = logicRegex.findFirstMatchIn(line)

    if (inMatch.isDefined) {
      val m = inMatch.get
      val (value, bot) = (m.group(1).toInt, m.group(2).toInt)
      (bot, updateBot(bots, bot, b => b.copy(vals = b.vals :+ value)))
    } else {
      val m = logicMatch.get
      val Seq(source, lowDestI, highDestI) = Seq(1, 3, 5).map(m.group(_).toInt)
      val Seq(lowDestType, highDestType) = Seq(2, 4).map(m.group)
      val Seq(lowDest, highDest) = Seq((lowDestType, lowDestI), (highDestType, highDestI)).map {
        case ("bot", i) => BotDest(i)
        case ("output", i) => BinDest(i)
      }
      (source, updateBot(bots, source, b => b.copy(lowDest = Some(lowDest), highDest = Some(highDest))))
    }
  }

  private def triggerTransfer(bots: Map[Int, Bot], bins: Map[Int, Int], botId: Int): (Map[Int, Bot], Map[Int, Int]) = {
    val bot = bots(botId)
    if (bot.vals.sorted == Seq(17, 61)) {
      println(s"result: $botId")
    }
    if (bot.vals.length == 2 && bot.highDest.isDefined && bot.lowDest.isDefined) {
      val highest = bot.vals.max
      val lowest = bot.vals.min
      val (bots2, bins2) = transferValue(bots, bins, bot, highest, bot.highDest.get)
      val (bots3, bins3) = transferValue(bots2, bins2, bot, lowest, bot.lowDest.get)
      val (bots4, bins4) = (bots3.updated(botId, bot.copy(vals = Vector())), bins3)
      val (bots5, bins5) = bot.highDest.get match {
        case BotDest(i) => triggerTransfer(bots4, bins4, i)
        case _ => (bots4, bins4)
      }
      val (bots6, bins6) = bot.lowDest.get match {
        case BotDest(i) => triggerTransfer(bots5, bins5, i)
        case _ => (bots5, bins5)
      }
      (bots6, bins6)
    } else {
      (bots, bins)
    }
  }

  /**
    * @note: doesn't remove the value from fromBot
    */
  private def transferValue(bots: Map[Int, Bot], bins: Map[Int, Int], fromBot: Bot, value: Int, dest: Dest) = {
    dest match {
      case BotDest(i) =>
        val bot = bots.getOrElse(i, Bot(Vector(), None, None))
        val botsWithMovedHighest = bots.updated(i, bot.copy(vals = bot.vals :+ value))
        (botsWithMovedHighest, bins)
      case BinDest(i) =>
        (bots, bins.updated(i, value))
    }
  }

  private def updateBot(bots: Map[Int, Bot], bot: Int, f: (Bot => Bot)): Bot = {
    f(bots.getOrElse(bot, Bot(Vector(), None, None)))
  }
}
