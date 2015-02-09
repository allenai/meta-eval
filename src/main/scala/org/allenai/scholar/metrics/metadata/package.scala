package org.allenai.scholar.metrics

import com.typesafe.config.ConfigFactory
import scala.sys.process.Process
import scala.xml.NodeSeq

import java.io.{ ByteArrayInputStream, FileWriter, BufferedWriter, File }

package object metadata {
  val config = ConfigFactory.load()
  val root = config.getString("root")
  val dataHome = s"$root/${config.getString("data.home")}"
  val aclHome = s"$dataHome/${config.getString("data.acl.home")}"
  val aclPdfDir = s"$aclHome/${config.getString("data.acl.pdfDirectory")}"
  val aclMetadata = s"$aclHome/${config.getString("data.acl.metadata")}"

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

  /** Some convenient conversions for an XML NodeSeq.
    * @param n The XML NodeSeq.
    */
  implicit class XmlNodeSeqImplicits(n: NodeSeq) {
    def toLowerCaseText: String = n.text.toLowerCase

    /** Lower case, then trim all non-letters to the right.
      * @return The lower-cased and trimmed output.
      */
    def toLowerCaseTextTrimNonLetters: String = {
      def trim(s: String): String = if (s.isEmpty) s
      else s.last match {
        case c if c < 'a'.toInt || c > 'z'.toInt => trim(s.substring(0, s.size - 1))
        case _ => s
      }

      trim(n.toLowerCaseText)
    }
  }

}
