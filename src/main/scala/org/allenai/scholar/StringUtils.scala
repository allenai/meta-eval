package org.allenai.scholar

import java.time.Year

import org.apache.commons.lang3.StringUtils._

object StringUtils {

  val whiteSpaceRegex = """\s+""".r
  val nonAsciiRegex = """[^\p{ASCII}]""".r
  val punctuationRegex = """[\p{Punct}\*’“”→–—]""".r

  implicit class StringImplicits(str: String) {

    /** @return Trim white spaces, lower case, then strip the accents.
      */
    def normalized: String = whiteSpaceRegex.replaceAllIn(stripAccents(str.toLowerCase.trim), " ")

    def splitOnWhitespace: Array[String] = whiteSpaceRegex.split(str)

    def removePunctuation: String = punctuationRegex.replaceAllIn(str, " ")

    def collapseWhitespace: String = whiteSpaceRegex.replaceAllIn(str, " ")

    /** @param filter Determine if a character is blacklisted and should be trimmed.
      * @return String with blacklisted chars trimmed from the right.
      */
    def trimRight(filter: Char => Boolean): String = {
      var i = str.size - 1
      while (i >= 0 && filter(str.charAt(i))) {
        i -= 1
      }
      str.substring(0, math.max(0, i + 1))
    }

    /** @return Given full name such as "Doe, John A.", returns the last name assuming
      *         that it's the word before the comma.
      */
    def lastNameFromFull(): String = str.trim.takeWhile(_ != ',')

    /** @return Trim non-letter chars from the beginning and end
      */
    def trimNonAlphabetic(): String = str.dropWhile(c =>
      !Character.isAlphabetic(c)).trimRight(c => !Character.isAlphabetic(c))

    /** @param chars String containing the blacklist chars.
      * @return Trim characters from the right that belongs to a blacklist.
      */
    def trimChars(chars: String): String = str.trimRight(c => chars.contains(c))
  }

}
