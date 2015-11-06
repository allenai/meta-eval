package org.allenai.scholar.eval

import java.io.File

import org.allenai.common.{ Logging, Resource }
import org.allenai.scholar.metrics.{ ErrorAnalysis, ErrorAnalysisIo, Example, PrecisionRecall }
import scopt.OptionParser

import scala.io.Source

object Main extends App with Logging {

  def run(cfg: Config): Unit = {
    val analysis =
      if (cfg.input.isDirectory) {
        (for (predictionsFile <- cfg.input.listFiles if !predictionsFile.isDirectory) yield {
          val truthFile = new File(cfg.truth, predictionsFile.getName)
          if (!truthFile.exists) {
            logger.warn(s"No truth file named ${predictionsFile.getName}. Skipping")
            None
          } else {
            logger.debug(s"Comparing $predictionsFile to $truthFile")
            val metricName = predictionsFile.getName.takeWhile(_ != '.')
            val examples = readExamples(predictionsFile, truthFile)
            Some(ErrorAnalysis(metricName, PrecisionRecall.measurePR(examples.map(_._2)), examples))
          }
        }).flatten.toIterable
      } else {
        val examples = readExamples(cfg.input, cfg.truth)
        List(ErrorAnalysis(cfg.input.getName.takeWhile(_ != '.'), PrecisionRecall.measurePR(examples.map(_._2)), examples))
      }
    logger.info("")
    logger.info(s"Metric\tPrecision\tRecall")
    for (a <- analysis) {
      val pString = a.pr.precision.map(x => "%.3f".format(x)).getOrElse("")
      val rString = a.pr.recall.map(x => "%.3f".format(x)).getOrElse("")
      logger.info(s"${a.metricName}\t$pString\t$rString")
    }
    logger.info("")
    logger.info(s"Details written to ${new File(cfg.output, "details")}")
    ErrorAnalysisIo.write(analysis, cfg.output)
    logger.debug("Finished")
  }

  case class Config(
    input: File = null,
    truth: File = new File(new File(System.getProperty("user.dir")), "src/main/resources/gold/acl"),
    output: File = new File(new File(System.getProperty("user.dir")), "output")
  )

  val parser = new OptionParser[Config]("meta-eval") {
    opt[File]("input")
      .text("Input directory containing predictions")
      .required()
      .action { (x, c) => c.copy(input = x) }
      .validate { f => if (f.exists) success else failure(s"Input dir $f not found") }

    opt[File]("truth")
      .text("Directory containing truth data")
      .action { (x, c) => c.copy(truth = x) }
      .validate { f => if (f.exists) success else failure(s"Truth dir $f not found") }

    opt[File]("output")
      .text("Output directory for report")
      .action { (x, c) => c.copy(output = x) }

    checkConfig { cfg: Config =>
      val Config(input, truth, output) = cfg
      if (input.isDirectory && truth.isDirectory) {
        success
      } else {
        failure("Input/Truth must both be directories")
      }
    }

  }

  parser.parse(args, Config()) match {
    case Some(cfg) =>
      run(cfg)
    case _ => System.exit(1)
  }

  def readExamples(predictionFile: File, truthFile: File) = {
    def readLabels(s: String) = {
      val fields = s.split('\t')
      (fields(0), fields.drop(1))
    }
    Resource.using(Source.fromFile(truthFile)) { truthInput =>
      Resource.using(Source.fromFile(predictionFile)) { predInput =>
        val truth = truthInput.getLines.map(readLabels).toMap
        val examples = for (line <- predInput.getLines.toList) yield {
          val (id, predictions) = readLabels(line)
          (id, Example(truth.getOrElse(id, Array.empty[String]), predictions))
        }
        val missingPredictions =
          for ((id, trueLabels) <- truth -- examples.map(_._1)) yield {
            (id, Example(trueLabels, Array.empty[String]))
          }
        examples ++ missingPredictions
      }
    }
  }

}

