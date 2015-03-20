package org.allenai.scholar.metrics.metadata

import org.apache.commons.lang3.StringUtils._
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import java.time.Year

/** A collection of text processing, normalization, and
  * Jsoup-based utilities packaged in implicits to reduce boiler plate.
  */
object Parser {

  implicit class StringImplicits(str: String) {

    /** @return Trim white spaces, lower case, then strip the accents.
      */
    def normalize(): String = stripAccents(str.trim.toLowerCase)

    private def trimRight(s: String, filter: Char => Boolean): String = if (s.isEmpty) {
      s
    } else {
      s.last match {
        case c if filter(c) => trimRight(s.substring(0, s.size - 1), filter)
        case _ => s
      }
    }

    /** @param filter Determine if a character is blacklisted and should be trimmed.
      * @return String with blacklisted chars trimmed from the right.
      */
    def trimRight(filter: Char => Boolean): String = trimRight(str, filter)

    /** @return Given full name such as "Doe, John A.", returns the last name assuming that it's the word before the comma.
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

  object ElementsImplicit {

    import scala.collection.JavaConverters._
    import scala.language.implicitConversions

    /** Convenient implicit: no need for .asScala any time calling Jsoup's select. */
    implicit def elementsToSeq(elms: Elements): collection.mutable.Buffer[Element] = elms.asScala
  }

  implicit class JsoupElementsImplicits(e: Element) {

    import ElementsImplicit._

    def extractNormalize(path: String): String = e.select(path).headOption match {
      case Some(v) => v.text.normalize
      case None => ""
    }

    def extractName(namePath: String): String =
      extractNormalize(namePath).trimNonLowerCaseLetters

    def extractTitle(path: String): String = extractNormalize(path)

    def extractBibTitle(path: String): String =
      e.extractNormalize(path).trimChars(",.")

    def extractYear(path: String, get: Element => String): Year =
      e.select(path).headOption match {
        case Some(d) => get(d).extractYear
        case None => yearZero
      }

  }

}
