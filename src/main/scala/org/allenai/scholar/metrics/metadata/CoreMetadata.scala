package org.allenai.scholar.metrics.metadata

import scala.xml.{ Elem, NodeSeq, XML }

import java.io.File

/** Research papers' core metadata.
  * @param title The paper's title.
  * @param authorFullNames Sorted list of authors' full names. Full names are formatted as Smith, John A.
  * @param authorLastNames Sorted list of authors' last names
  */
case class CoreMetadata(
  title: String,
  authorFullNames: List[String],
  authorLastNames: List[String]
)

object CoreMetadata {

  /** Function that parses an XML file to produce core metadata.
    * @param file The XML file
    * @param titleExtractor Title extracting function.
    * @param authorExtractor Authors extracting function.
    * @param nameExtractor Names extracting function.
    * @return The paper's core metadata.
    */
  def parseCoreMetadata(
    file: File,
    titleExtractor: Elem => String,
    authorExtractor: Elem => NodeSeq,
    nameExtractor: NodeSeq => Seq[(String, String)]
  ): CoreMetadata = {
    println(s"processing ${file.getName}")
    val xml = XML.loadFile(file)
    val (fullNames, lastNames) = nameExtractor(authorExtractor(xml)).unzip
    CoreMetadata(titleExtractor(xml), fullNames.toList.sorted, lastNames.toList.sorted)
  }

  /** Function that extracts core metadata from a Grobid XML output file.
    * Names are lower-cased and trimmed of non-letter at the end.
    * @param f Grobid XML output file.
    * @return The paper's core metadata.
    */
  def parseGrobidCoreMetadata(f: File): CoreMetadata = parseCoreMetadata(
    file = f,
    titleExtractor = xml => (xml \\ "teiHeader" \\ "fileDesc" \\ "titleStmt" \\ "title")
    .toLowerCaseText,
    authorExtractor = xml => (xml \\ "teiHeader" \\ "fileDesc" \\ "sourceDesc" \\ "biblStruct" \\
    "analytic" \\ "author"),
    nameExtractor = authors => authors.map {
    a =>
      val lastName = (a \\ "persName" \\ "surname").toLowerCaseTextTrimNonLetters
      val foreNames = (a \\ "persName" \\ "forename").map(_.toLowerCaseTextTrimNonLetters)
      val fullName = if (foreNames.nonEmpty) lastName + ", " + foreNames.mkString(" ") else lastName
      (fullName, lastName)
  }
  )

  /** Function that extracts core metadata from a Metatagger XML output file.
    * Names are lower-cased and trimmed of non-letter at the end.
    * @param f Metatagger XML output file.
    * @return The paper's core metadata.
    */
  def parseMetataggerCoreMetadata(f: File): CoreMetadata = parseCoreMetadata(
    file = f,
    titleExtractor = xml => (xml \\ "document" \\ "content" \\ "headers" \\ "title")
    .toLowerCaseText,
    authorExtractor = xml => (xml \\ "document" \\ "content" \\ "headers" \\ "authors" \\ "author"),
    nameExtractor = authors => authors.map {
    a =>
      val lastName = (a \\ "author-last").toLowerCaseTextTrimNonLetters
      var fullName = lastName
      val firstName = (a \\ "author-first").toLowerCaseTextTrimNonLetters
      if (firstName.nonEmpty) fullName = fullName + ", " + firstName
      val middleNameSeq = a \\ "author-middle"
      if (middleNameSeq.nonEmpty) {
        val middleName = middleNameSeq.toLowerCaseTextTrimNonLetters
        if (middleName.nonEmpty) fullName = fullName + " " + middleName
      }

      (fullName, lastName)
  }
  )
}

