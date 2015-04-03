package org.allenai.scholar.metrics

import scala.sys.process.Process

import java.io._

package object metadata {
  val yearZero = java.time.Year.of(0)

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
  ): Unit = {
    import scala.language.postfixOps
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
  def writeToFile(lines: Iterable[String], fileName: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(new File(fileName)))
    bw.write(lines.mkString("\n"))
    bw.close()
  }

  def writeToFile(fileName: String)(write: PrintWriter => Unit) = {
    val w = new PrintWriter(new FileWriter(new File(fileName)))
    write(w)
    w.close()
  }
}
