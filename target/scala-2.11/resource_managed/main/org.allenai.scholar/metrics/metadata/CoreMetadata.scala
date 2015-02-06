package org.allenai.scholar.metrics.metadata

import scala.xml.{Elem, NodeSeq, XML}

import java.io.File

case class CoreMetadata(
  title: String,
  authorFullNames: List[String],
  authorLastNames: List[String])

object CoreMetadata {

  def parseCoreMetadata(
    file: File,
    titleExtractor: Elem => String,
    authorExtractor: Elem => NodeSeq,
    nameExtractor: NodeSeq => Seq[(String, String)]): CoreMetadata = {
    val xml = XML.loadFile(file)
    val (fullNames, lastNames) = nameExtractor(authorExtractor(xml)).unzip
    CoreMetadata(titleExtractor(xml), fullNames.toList.sorted, lastNames.toList.sorted)
  }

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

