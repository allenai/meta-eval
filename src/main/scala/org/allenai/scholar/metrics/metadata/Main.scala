package org.allenai.scholar.metrics.metadata

import java.io.File
import java.nio.file.{ Files, Paths }

object Main extends App {
  /** Run only Grobid's processHeader for now, not fullText.
    * https://github.com/kermitt2/grobid/wiki/Grobid-batch-quick-start
    */
  def runGrobid() = {
    val processCmd = s"""java -Xmx4096m
                         -jar $grobidJar -gH $grobidHome
                         -gP $grobidProperties
                         -dIn $aclPdfDir
                         -dOut $grobidAclExtracted
                         -exe processFullText"""
    runProcess(processCmd)
  }

  def evalGrobid() = {
    Eval.evalGrobid(new File(grobidAclExtracted).listFiles, aclMetadata, aclCitationEdges)
  }

  def psToTextOutputFile(input: File) = s"$pstotextAclExtracted/${input.getName}.xml"

  def runPsToText() = {
    def processCmd(input: File) =
      s"""$pstotextHome/bin/pstotext
           -output ${psToTextOutputFile(input)}
           -ligatures $pstotextHome/bin/ligatures.txt
           ${input.getAbsolutePath}"""

    val startTime = System.currentTimeMillis()
    val inputs = new File(aclPdfDir).listFiles
    inputs.foreach(input => runProcess(processCmd(input), time = false))
    println(s"Time elapsed in milliseconds: ${System.currentTimeMillis() - startTime}")
  }

  def runMetatagger() = {
    val inputs = new File(pstotextAclExtracted).listFiles
    val metataggerInput = inputs.flatMap { input =>
      val output = s"$metataggerAclExtracted/${input.getName}.tagged.xml"

      // skip if output file exists
      if (Files.exists(Paths.get(output))) None
      else Some(s"${input.getPath} -> $output")
    }

    runProcess(
      s"bin/runcrf",
      cwd = Some(metataggerHome),
      input = Some(metataggerInput.mkString("\n"))
    )
  }

  def evalMetatagger() = {
    // Filter out files that are too small, a sign that metatagger failed
    val files = new File(metataggerAclExtracted).listFiles.filter(_.length > 1000)
    Eval.evalMetatagger(files, aclMetadata, aclCitationEdges)
  }

  val cmds = this.getClass.getDeclaredMethods.map(m => m.getName -> m).toMap

  cmds get args(0) match {
    case Some(m) =>
      println(s"Invoking ${m.getName}")
      m.invoke(this)
    case _ => println(s"Unrecognized cmd: ${args(0)}")
  }
}
