package org.allenai.scholar.metrics.metadata

import java.io.File

object Main extends App {
  def runGrobid() = {
    val processCmd = s"""java -Xmx1024m
                         -jar $grobidJar -gH $grobidHome
                         -gP $grobidProperties
                         -dIn $aclPdfDir
                         -dOut $grobidAclExtracted
                         -exe processHeader"""
    runProcess(processCmd)
  }

  def evalGrobid() = {
    import PaperMetadata._
    val files = new File(grobidAclExtracted).listFiles
    val groundTruthMetadata = convertToCore(fromJsonLinesFile(aclMetadata))
    Eval.evalGrobid(files, groundTruthMetadata)
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
    val metataggerInput = inputs.map { input =>
      s"${input.getPath} -> $metataggerAclExtracted/${input.getName}.tagged.xml"
    }

    runProcess(
      s"bin/runcrf",
      cwd = Some(metataggerHome),
      input = Some(metataggerInput.mkString("\n")))
  }

  def evalMetatagger() = {
    import PaperMetadata._
    // Filter out files that are too small, a sign that metatagger failed
    val files = new File(metataggerAclExtracted).listFiles.filter(_.length > 1000)
    val groundTruthMetadata = convertToCore(fromJsonLinesFile(aclMetadata))
    Eval.evalMetatagger(files, groundTruthMetadata)
  }

  val cmds = this.getClass.getDeclaredMethods.map(m => m.getName -> m).toMap

  cmds get args(0) match {
    case Some(m) => {
      println(s"Invoking ${m.getName}")
      m.invoke(this)
    }
    case _ => println(s"Unrecognized cmd: ${args(0)}")
  }
}
