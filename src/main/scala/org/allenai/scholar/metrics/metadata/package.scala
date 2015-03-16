package org.allenai.scholar.metrics

import com.typesafe.config.ConfigFactory
import org.apache.commons.lang3.StringUtils._
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

import scala.sys.process.Process

import java.io.{ BufferedWriter, ByteArrayInputStream, File, FileWriter }
import java.time.Year

package object metadata {
  val config = ConfigFactory.load()
  val root = config.getString("root")
  val dataHome = s"$root/${config.getString("data.home")}"
  val aclHome = s"$dataHome/${config.getString("data.acl.home")}"
  val aclPdfDir = s"$aclHome/${config.getString("data.acl.pdfDirectory")}"
  val aclMetadata = s"$aclHome/${config.getString("data.acl.metadata")}"
  val aclCitationEdges = s"$aclHome/${config.getString("data.acl.citationEdges")}"

  val grobidRoot = s"$root/${config.getString("grobid.root")}"
  val grobidHome = s"$grobidRoot/grobid-home"
  val grobidJar = new File(s"$grobidRoot/grobid-core/target")
    .listFiles
    .filter(_.getName.endsWith("one-jar.jar"))
    .head
    .getAbsolutePath
  val grobidProperties = s"$grobidHome/config/grobid.properties"

  val pstotextHome = s"$root/${config.getString("pstotext.home")}"
  val metataggerHome = s"$root/${config.getString("metatagger.home")}"
  val aclExtracted = s"$aclHome/${config.getString("data.acl.extracted")}"
  val grobidAclExtracted = s"$aclExtracted/grobid"
  val pstotextAclExtracted = s"$aclExtracted/pstotext"
  val metataggerAclExtracted = s"$aclExtracted/metatagger"

  val defaultPublishedYear = Year.parse("0000")

  /** Run a shell process, optionally time it.
    * @param processCmd The command string to execute.
    * @param time Whether to print elapsed time.
    * @param input If defined, use as stdin to the process.
    * @param cwd If defined, run the process in this directory.
    */
  def runProcess(
    processCmd: String,
    time: Boolean = true,
    input: Option[String] = None,
    cwd: Option[String] = None
  ) = {

    println(s"Running command: $processCmd")
    val proc = cwd match {
      case Some(d) => Process(processCmd, new File(d))
      case _ => Process(processCmd)
    }

    val startTime = if (time) System.currentTimeMillis() else 0
    input match {
      case Some(inputStr) => proc #< new ByteArrayInputStream(inputStr.getBytes) !
      case _ => proc !
    }
    if (time) println(s"Time elapsed in milliseconds: ${System.currentTimeMillis() - startTime}")
  }

  /** Write a set of lines to a file.
    * @param lines The lines.
    * @param fileName The name of the output file.
    */
  def writeToFile(lines: Iterable[String], fileName: String) = {
    val bw = new BufferedWriter(new FileWriter(new File(fileName)))
    bw.write(lines.mkString("\n"))
    bw.close()
  }

  implicit class StringImplicits(str: String) {
    def normalize() = stripAccents(str.trim.toLowerCase)

    def trimRight(s: String, filter: Char => Boolean): String = {
      if (s.isEmpty) s
      else s.last match {
        case c if filter(c) => trimRight(s.substring(0, s.size - 1), filter)
        case _ => s
      }
    }

    def trimRight(filter: Char => Boolean): String = trimRight(str, filter)

    def lastNameFromFull() = str.trim.takeWhile(_ != ',')

    def buildFullName(first: String, middle: String, initial: Boolean = false) = {
      def format(name: String) = if (initial) name(0) else name
      var full = str
      if (first.nonEmpty) full = full + s", ${format(first)}"
      if (middle.nonEmpty) full = full + s" ${format(middle)}"
      full
    }
  }

  object ElementsImplicit {

    import scala.collection.JavaConverters._

    /** Convenient implicit: no need for .asScala any time calling jsoup's select. */
    implicit def elementsToSeq(elms: Elements): collection.mutable.Buffer[Element] = elms.asScala
  }

  /** Some convenient conversions for an XML NodeSeq.
    * @param e The XML NodeSeq.
    */
  implicit class JsoupElementsImplicits(e: Element) {

    import org.allenai.scholar.metrics.metadata.ElementsImplicit._

    def toLowerCaseText(): String = e.text.toLowerCase

    /** Lower case, then trim all non-letters to the right.
      * @return The lower-cased and trimmed output.
      */
    def toLowerCaseTextTrimNonLetters: String =
      e.text.normalize.trimRight(c => c < 'a'.toInt || c > 'z'.toInt)

    /** Lower case, then trim all characters end the end that belong to a blacklist
      * @param chars The string containing the blacklisted chars.
      * @return The trimmed string.
      */
    def toLowerCaseTextTrimChars(chars: String): String =
      e.text.normalize.trimRight(c => chars.contains(c))

    def extractName(namePath: String) = e.select(namePath).headOption match {
      case Some(n) => n.toLowerCaseTextTrimNonLetters
      case None => ""
    }

    def extractLower(path: String) = e.select(path).headOption match {
      case Some(v) => v.toLowerCaseText
      case None => ""
    }

    def extractTitle(path: String) = extractLower(path)

    def extractLowerTrimChars(path: String, chars: String) =
      e.select(path).headOption match {
        case Some(v) => v.toLowerCaseTextTrimChars(chars)
        case None => ""
      }

    def extractBibTitle(path: String) = e.extractLowerTrimChars(path, ",.")

    def extractYear(path: String, get: Element => String) =
      e.select(path).headOption match {
        case Some(d) => "\\d{4}".r.findFirstIn(get(d)) match {
          case Some(y) => Year.parse(y)
          case None => defaultPublishedYear
        }
        case None => defaultPublishedYear
      }

  }

}
