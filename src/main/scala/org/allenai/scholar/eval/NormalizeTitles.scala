package org.allenai.scholar.eval

import org.allenai.common.Resource
import org.allenai.scholar.fields.{ Title, Author }

import java.io.PrintWriter
import scala.io.Source

object NormalizeTitles extends App {
  if (args.length != 2) {
    System.err.println(
      s"Usage: ${this.getClass.getSimpleName} <titleExact> <titleNormalized>\n" +
        """|  <titleExact> is a tsv file that's read as input
          |  <titleNormalized> is the output
       """.stripMargin
    )
    System.exit(1)
  }

  val titleExactFilename = args(0)
  val titleNormalizedFilename = args(1)

  Resource.using(Source.fromFile(titleExactFilename)) { src =>
    Resource.using(new PrintWriter(titleNormalizedFilename, "UTF-8")) { titleNormalized =>
      src.getLines().foreach { line =>
        val parts = line.split("\t")

        val paperId = parts(0)
        titleNormalized.print(paperId)

        parts.drop(1).foreach { part =>
          val title = Title(part)

          titleNormalized.print('\t')
          titleNormalized.print(title.normalized.text)
        }

        titleNormalized.println()
      }
    }
  }
}
