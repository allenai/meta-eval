package org.allenai.scholar.metrics.metadata

import org.allenai.scholar.metrics.metadata.CoreMetadata.{ GrobidParser, MetataggerParser }

import java.io.File
import java.nio.file.{ Files, Paths }

object Main extends App {
  /** Run only Grobid's processHeader for now, not fullText.
    * https://github.com/kermitt2/grobid/wiki/Grobid-batch-quick-start
    */
  def runGrobid(): Unit = {
    val processCmd = s"""java -Xmx4096m
                         -jar $grobidJar -gH $grobidHome
                         -gP $grobidProperties
                         -dIn $aclPdfDir
                         -dOut $grobidAclExtracted
                         -exe processFullText"""
    runProcess(processCmd)
  }

  def evalGrobid(): Unit = {
    Eval(
      algoName = "Grobid",
      taggedFiles = new File(grobidAclExtracted).listFiles,
      taggedFileParser = GrobidParser.parseCoreMetadata
    ).run(aclMetadata, aclCitationEdges, Some(aclIdWhiteList))
  }

  def runPsToText(): Unit = {
    def psToTextOutputFile(input: File): String = s"$pstotextAclExtracted/${input.getName}.xml"
    def processCmd(input: File): String =
      s"""$pstotextHome/bin/pstotext
           -output ${psToTextOutputFile(input)}
           -ligatures $pstotextHome/bin/ligatures.txt
           ${input.getAbsolutePath}"""

    val startTime = System.currentTimeMillis()
    val inputs = new File(aclPdfDir).listFiles
    inputs.foreach(input => runProcess(processCmd(input), time = false))
    println(s"Time elapsed in milliseconds: ${System.currentTimeMillis() - startTime}")
  }

  def runMetatagger(): Unit = {
    val inputs = new File(pstotextAclExtracted).listFiles
    val metataggerInput = inputs.flatMap { input =>
      val output = s"$metataggerAclExtracted/${input.getName}.tagged.xml"

      // skip if output file exists
      if (Files.exists(Paths.get(output))) None else Some(s"${input.getPath} -> $output")
    }

    runProcess(
      s"bin/runcrf",
      cwd = Some(metataggerHome),
      input = Some(metataggerInput.mkString("\n"))
    )
  }

  def evalMetatagger(): Unit = {
    Eval(
      algoName = "Metatagger",
      taggedFiles = new File(metataggerAclExtracted).listFiles,
      taggedFileParser = MetataggerParser.parseCoreMetadata
    ).run(aclMetadata, aclCitationEdges, Some(aclIdWhiteList))
  }

  val cmds = this.getClass.getDeclaredMethods.map(m => m.getName -> m).toMap

  cmds get args(0) match {
    case Some(m) =>
      println(s"Invoking ${m.getName}")
      m.invoke(this)
    case _ => println(s"Unrecognized cmd: ${args(0)}")
  }
}
