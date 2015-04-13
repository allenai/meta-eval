package org.allenai.scholar.metrics.metadata

import com.typesafe.config.ConfigFactory

import java.io.File

object Config {
  val config = ConfigFactory.load()
  val root = config.getString("root")
  val dataHome = s"$root/${config.getString("data.home")}"
  val aclHome = s"$dataHome/${config.getString("data.acl.home")}"
  val aclPdfDir = s"$aclHome/${config.getString("data.acl.pdfDirectory")}"
  val aclMetadata = s"$aclHome/${config.getString("data.acl.metadata")}"
  val aclCitationEdges = s"$aclHome/${config.getString("data.acl.citationEdges")}"
  val aclIdWhiteList = s"$aclHome/${config.getString("data.acl.idWhiteList")}"

  val grobidRoot = s"$root/${config.getString("grobid.root")}"
  val grobidHome = s"$grobidRoot/grobid-home"
  lazy val grobidJar = new File(s"$grobidRoot/grobid-core/target")
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
  val verboseLabelFormat = config.getBoolean("verboseLabelFormat")
}
