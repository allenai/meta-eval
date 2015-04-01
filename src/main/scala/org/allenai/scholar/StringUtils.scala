package org.allenai.scholar

import java.time.Year

import org.allenai.scholar.metrics.metadata._
import org.apache.commons.lang3.StringUtils._

object StringUtils {

  val whiteSpaceRegex = """\s+""".r
  val nonAsciiRegex = """[^\p{ASCII}]""".r
  val punctuationRegex = """[\p{Punct}]""".r

  implicit class StringImplicits(str: String) {

    /** @return Trim white spaces, lower case, then strip the accents.
      */
    def normalize(): String = stripAccents(str.trim.toLowerCase)

    def splitOnWhitespace(): Array[String] = whiteSpaceRegex.split(str)

    def removePunctuation(): String = punctuationRegex.replaceAllIn(str, " ")

    /** @param filter Determine if a character is blacklisted and should be trimmed.
      * @return String with blacklisted chars trimmed from the right.
      */
    def trimRight(filter: Char => Boolean): String = {
      var i = str.size - 1
      while (i >=0 && filter(str.charAt(i))) {
        i -= 1
      }
      str.substring(0,math.max(0,i+1))
    }

    /** @return Given full name such as "Doe, John A.", returns the last name assuming
      *         that it's the word before the comma.
      */
    def lastNameFromFull(): String = str.trim.takeWhile(_ != ',')

    /** @return Trim non-letter chars from the right of a lower-cased string.
      */
    def trimNonLowerCaseLetters(): String = str.trimRight(c => c < 'a'.toInt || c > 'z'.toInt)

    /** @param chars String containing the blacklist chars.
      * @return Trim characters from the right that belongs to a blacklist.
      */
    def trimChars(chars: String): String = str.trimRight(c => chars.contains(c))

    /** @param first First name, e.g. "John".
      * @param middle Middle Name, e.g. "Alan" or "A.".
      * @param initial If true, take only the initials from first and middle names.
      * @return The full name in format "Doe, John A.", built from last name.
      */
    def buildFullName(first: String, middle: String, initial: Boolean = false): String = {
      def format(name: String): String = if (initial) name(0).toString else name
      var full = str
      if (first.nonEmpty) full = full + s", ${format(first)}"
      if (middle.nonEmpty) full = full + s" ${format(middle)}"
      full
    }

    def extractYear(): Year = "\\d{4}".r.findFirstIn(str) match {
      case Some(y) => Year.parse(y)
      case None => yearZero
    }
  }



}
