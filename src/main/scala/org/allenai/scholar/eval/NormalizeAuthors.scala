package org.allenai.scholar.eval

import org.allenai.common.Resource
import org.allenai.scholar.fields.Author

import java.io.{ PrintWriter, File }
import scala.io.Source

object NormalizeAuthors extends App {
  if (args.length != 3) {
    System.err.println(
      s"Usage: ${this.getClass.getSimpleName} <fullNameExact> <lastNameExact> <lastNameNormalized>\n" +
        """|  <fullNameExact> is a tsv file that's read as input
         |  <lastNameExact> and <lastNameNormalized> are outputs
      """.stripMargin
    )
    System.exit(1)
  }

  val fullNameExactFilename = args(0)
  val lastNameExactFilename = args(1)
  val lastNameNormalizedFilename = args(2)

  Resource.using(Source.fromFile(fullNameExactFilename)) { src =>
    Resource.using2(
      new PrintWriter(lastNameExactFilename, "UTF-8"),
      new PrintWriter(lastNameNormalizedFilename, "UTF-8")
    ) {
        case (lastNameExact, lastNameNormalized) =>
          src.getLines().foreach { line =>
            val parts = line.split("\t")

            val paperId = parts(0)
            lastNameExact.print(paperId)
            lastNameNormalized.print(paperId)

            parts.drop(1).foreach { part =>
              val author = Author.parse(part)

              lastNameExact.print('\t')
              lastNameExact.print(author.lastName)

              lastNameNormalized.print('\t')
              lastNameNormalized.print(author.lastNameNormalized)
            }

            lastNameExact.println()
            lastNameNormalized.println()
          }
      }
  }
}
