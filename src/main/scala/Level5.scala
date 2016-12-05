import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

import scala.collection.mutable

/**
  * Created by olivierdeckers on 05/12/2016.
  */
object Level5 {

  def md5(s:String): String = {
    val digest = MessageDigest.getInstance("MD5").digest(s.getBytes())
    DatatypeConverter.printHexBinary(digest).toLowerCase()
  }

  def main(args: Array[String]): Unit = {
    println(findNextChar)
  }

  private def findNextChar: Option[Char] = {
    val doorId = "ojvtpuvg"
//    val doorId = "abc"
    var i = BigInt(0)
    val result = mutable.MutableList.fill[Option[Char]](8)(None)
    while(true) {
//      if (i % 100000 == 0) {
//        println(i)
//      }
      val hash = md5(doorId + i)
      if (hash.substring(0, 5) == "00000") {
//        println(i, hash.charAt(5))
        val char = hash.charAt(6)
        val pos = hash.charAt(5)
        if (pos.isDigit) {
          val intPos = pos.toString.toInt
          if (intPos < 8 && intPos >= 0 && result(intPos).isEmpty) {
            result(intPos) = Some(char)
            println(result.mkString(","))
          }
        }
        //return Some(hash.charAt(5))
      }
      i += 1
    }
    None
  }
}
