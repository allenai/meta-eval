package org.allenai.scholar.eval

import java.io.{ File, PrintWriter }

import org.allenai.common.Resource
import org.allenai.scholar.StringUtils._
import org.allenai.scholar.fields.{ Author, Title, Venue }
import spray.json.DefaultJsonProtocol._
import spray.json._

import java.nio.file.{ Paths, Files }
import java.util.stream.Collectors
import java.util.zip.GZIPInputStream
import scala.io.Source
import scala.collection.JavaConverters._

/** Run this to create .tsv files with different fields extracted based on a raw data file, whose format depends on the source
  */
trait CreateGoldData {
  def write(paperId: String, w: PrintWriter, data: String*) = {
    val nonEmptyData = data.filter(_.length > 0).sorted
    if (nonEmptyData.nonEmpty) {
      w.println(s"$paperId\t${nonEmptyData.mkString("\t")}")
    }
  }

  // Downloaded from http://ai2-s2-pipeline.s3.amazonaws.com/output/publish/2015-10-24_1100_f6c41/data/GoldDataForMetaEval.527cc65fdde19cc8.json
  def pipelineMetadataOutputFile = "/Users/rodneykinney/Downloads/GoldDataForMetaEval.527cc65fdde19cc8.json"
}

// Format is directory of JSON files produced by org.allenai.scienceparse.Parser
object DblpScienceParse extends App with CreateGoldData {
  case class SP(title: String, authors: List[String])
  implicit val fmt = jsonFormat2(SP)
  val inputDir = new File(args(0))
  val outputDir = new File(args(1))
  Resource.using(new PrintWriter(new File(outputDir, "titleExact.tsv"))) { titleExact =>
    Resource.using(new PrintWriter(new File(outputDir, "titleNormalized.tsv"))) { titleNormalized =>
      Resource.using(new PrintWriter(new File(outputDir, "authorFullNameExact.tsv"))) { authorFull =>
        Resource.using(new PrintWriter(new File(outputDir, "authorLastNameExact.tsv"))) { authorLast =>
          Resource.using(new PrintWriter(new File(outputDir, "authorLastNameNormalized.tsv"))) { authorLastNorm =>
            for (file <- inputDir.listFiles) {
              Resource.using(Source.fromFile(file)) { src =>
                val s = src.mkString
                if (s != "null") {
                  val json = s.parseJson.asJsObject
                  val title = if (json.fields("title").toString == "null") Title("") else Title(json.fields("title").asInstanceOf[JsString].value)
                  val authors = json.fields("authors").convertTo[List[String]]
                  val paperId = file.getName.takeWhile(_ != '.')
                  write(paperId, titleExact, title.text)
                  write(paperId, titleNormalized, title.normalized.text)
                  write(paperId, authorFull, authors: _*)
                  write(paperId, authorLast, authors.map(s => Author.parse(s).lastName): _*)
                  write(paperId, authorLastNorm, authors.map(s => Author.parse(s).lastNameNormalized): _*)
                }
              }
            }
          }
        }
      }
    }
  }
}

object DblpFromPipeline extends App with CreateGoldData {

  import MetadataParser._

  val keepId: String => Boolean = {
    val idFile = new File("src/main/resources/raw/dblp/ids.txt")
    val ids = Resource.using(Source.fromFile(idFile)) { src => src.getLines().toSet }
    s: String => ids(s)
  }

  val allClusters = Resource.using(Files.list(Paths.get("/Users/dirkg/temp/FinalPaperClusters.fa33b8c61c12c42b3e240e2dc7bc1e44591619aa"))) { javaPaths =>
    val paths = javaPaths.collect(Collectors.toList()).asScala.toIterator
    paths.filter(p => p.getFileName.toString.startsWith("part-") && p.getFileName.toString.endsWith(".gz")).flatMap { path =>
      println(path.toString + " ...")
      Resource.using(Source.fromInputStream(new GZIPInputStream(Files.newInputStream(path)))) { source =>
        source.getLines().map(parse).toList
      }
    }
  }

  val keptClusters = allClusters.filter { cluster => keepId(cluster.paperIds.head) }
  val clustersWithDblp = keptClusters.filter(_.dblp.isDefined)
  val clusters = clustersWithDblp.toSeq.sortBy(_.paperIds.head)

  Resource.using(new PrintWriter("src/main/resources/baseline/dblp/scienceParse/titleExact.tsv")) {
    spTitleExact =>
      Resource.using(new PrintWriter("src/main/resources/baseline/dblp/scienceParse/titleNormalized.tsv")) {
        spTitleNorm =>
          Resource.using(new PrintWriter("src/main/resources/baseline/dblp/scienceParse-highP/titleExact.tsv")) {
            sphpTitleExact =>
              Resource.using(new PrintWriter("src/main/resources/baseline/dblp/scienceParse-highP/titleNormalized.tsv")) {
                sphpTitleNorm =>
                  Resource.using(new PrintWriter("src/main/resources/baseline/dblp/grobid/titleExact.tsv")) {
                    gTitleExact =>
                      Resource.using(new PrintWriter("src/main/resources/baseline/dblp/grobid/titleNormalized.tsv")) {
                        gTitleNorm =>
                          Resource.using(new PrintWriter("src/main/resources/baseline/dblp/grobid/authorFullNameExact.tsv")) {
                            gAuthorFull =>
                              Resource.using(new PrintWriter("src/main/resources/baseline/dblp/grobid/authorLastNameNormalized.tsv")) {
                                gAuthorLastNorm =>
                                  Resource.using(new PrintWriter("src/main/resources/baseline/dblp/grobid/authorLastNameExact.tsv")) {
                                    gAuthorLast =>
                                      Resource.using(new PrintWriter("src/main/resources/gold/dblp/titleExact.tsv")) {
                                        goldTitle =>
                                          Resource.using(new PrintWriter("src/main/resources/gold/dblp/titleNormalized.tsv")) {
                                            goldTitleNorm =>
                                              Resource.using(new PrintWriter("src/main/resources/gold/dblp/authorFullNameExact.tsv")) {
                                                goldAuthor =>
                                                  Resource.using(new PrintWriter("src/main/resources/gold/dblp/authorLastNameExact.tsv")) {
                                                    goldAuthorLast =>
                                                      Resource.using(new PrintWriter("src/main/resources/gold/dblp/authorLastNameNormalized.tsv")) {
                                                        goldAuthorLastNorm =>
                                                          for {
                                                            Cluster(ids, acl, Some(dblp), grobid, scienceParse) <- clusters
                                                            paperId = ids.head
                                                          } {
                                                            write(paperId, goldTitle, dblp.title.text)
                                                            write(paperId, goldTitleNorm, dblp.title.normalized.text)
                                                            write(paperId, goldAuthor, dblp.authors: _*)
                                                            write(paperId, goldAuthorLast, dblp.authors.map(s => Author.parse(s).lastName): _*)
                                                            write(paperId, goldAuthorLastNorm, dblp.authors.map(s => Author.parse(s).lastNameNormalized): _*)

                                                            grobid.foreach {
                                                              case Grobid(title, _, _, authors) =>
                                                                write(paperId, gTitleExact, title.text)
                                                                write(paperId, gTitleNorm, title.normalized.text)
                                                                write(paperId, gAuthorFull, authors.map(_.toString): _*)
                                                                write(paperId, gAuthorLast, authors.map(_.lastName): _*)
                                                                write(paperId, gAuthorLastNorm, authors.map(_.lastNameNormalized): _*)
                                                            }

                                                            scienceParse.foreach {
                                                              case ScienceParse(title, _, authors, highP) =>
                                                                write(paperId, spTitleExact, title.text)
                                                                write(paperId, spTitleNorm, title.normalized.text)
                                                                if (highP) {
                                                                  write(paperId, sphpTitleExact, title.text)
                                                                  write(paperId, sphpTitleNorm, title.normalized.text)
                                                                }
                                                            }
                                                          }
                                                      }
                                                  }
                                              }
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }
}

object AclScienceParse extends App with CreateGoldData {

  import MetadataParser._

  val keepId = Source.fromFile("src/main/resources/raw/acl/ids.txt").getLines().toSet
  Resource.using(Source.fromFile(pipelineMetadataOutputFile)) { src =>
    Resource.using(new PrintWriter("src/main/resources/baseline/acl/scienceParse-highP/titleExact.tsv")) { highPTitleExact =>
      Resource.using(new PrintWriter("src/main/resources/baseline/acl/scienceParse-highP/titleNormalized.tsv")) { highPTitleNormalized =>
        Resource.using(new PrintWriter("src/main/resources/baseline/acl/scienceParse/titleExact.tsv")) { titleExactWriter =>
          Resource.using(new PrintWriter("src/main/resources/baseline/acl/scienceParse/titleNormalized.tsv")) { titleNormalizedWriter =>
            for {
              Cluster(ids, _, _, _, Some(meta)) <- src.getLines.map(parse)
              paperId <- ids.find(keepId)
            } {
              titleExactWriter.println(s"$paperId\t${meta.title.text}")
              titleNormalizedWriter.println(s"$paperId\t${meta.title.normalized.text}")
              if (meta.highPrecision) {
                highPTitleExact.println(s"$paperId\t${meta.title.text}")
                highPTitleNormalized.println(s"$paperId\t${meta.title.normalized.text}")
              }
            }
          }
        }
      }
    }
  }
}

// Raw format is PR measurement details from pipeline run, e.g. http://ai2-s2-pipeline.s3.amazonaws.com/output/publish/2015-10-24_1100_f6c41/data/GrobidMetadataAccuracy.789e4da9f63cfece.zip
object AclGrobid extends App with CreateGoldData {
  Resource.using(new PrintWriter("src/main/resources/baseline/acl/grobid/titleExact.tsv")) { titleExactWriter =>
    Resource.using(new PrintWriter("src/main/resources/baseline/acl/grobid/titleNormalized.tsv")) { titleNormalizedWriter =>
      Resource.using(Source.fromFile("src/main/resources/raw/grobid/titleExact-labels.txt")) { titleInput =>
        for {
          line <- titleInput.getLines.drop(1)
          fields = line.split('\t') if fields(2) == "pred"
          title = fields(3).drop(6).dropRight(1)
          paperId = fields(0)
        } {
          write(paperId, titleExactWriter, title)
          write(paperId, titleNormalizedWriter, Title(title).normalized.text)
        }
      }
    }
  }
  Resource.using(new PrintWriter("src/main/resources/baseline/acl/grobid/authorFullNameExact.tsv")) { authorFullExactWriter =>
    Resource.using(new PrintWriter("src/main/resources/baseline/acl/grobid/authorLastNameExact.tsv")) { authorLastExactWriter =>
      Resource.using(new PrintWriter("src/main/resources/baseline/acl/grobid/authorLastNameNormalized.tsv")) { authorLastNormalizedWriter =>
        Resource.using(Source.fromFile("src/main/resources/raw/grobid/authorFullName-labels.txt")) { authorInput =>
          val allAuthors = (for {
            line <- authorInput.getLines.drop(1)
            fields = line.split('\t') if fields(2) == "pred"
            author = Author.parse(fields(3))
            paperId = fields(0)
          } yield (paperId, author)).toList.groupBy(_._1).mapValues(_.map(_._2))
          for ((id, authors) <- allAuthors) {
            write(id, authorFullExactWriter, authors.map(_.toString): _*)
            write(id, authorLastExactWriter, authors.map(_.lastName): _*)
            write(id, authorLastNormalizedWriter, authors.map(_.lastName.normalized): _*)
          }
        }
      }
    }
  }
}

// Raw format is json, e.g. http://ai2-s2-sourcedata.s3.amazonaws.com/pipeline/metadata/metadata-2015-09-02.json
object AclGold extends App with CreateGoldData {
  val keepId = Source.fromFile("src/main/resources/raw/acl/ids.txt").getLines().toSet
  case class AclGold(paperId: String, title: Title, venue: Venue, year: Int, authors: List[Author])
  implicit val titleFmt = jsonFormat1(Title.apply)
  implicit val venueFmt = jsonFormat1(Venue.apply)
  implicit val authorFms = jsonFormat3(Author.apply)
  implicit val fmt = jsonFormat5(AclGold)
  Resource.using(new PrintWriter("src/main/resources/gold/acl/titleExact.tsv")) {
    titleExactWriter =>
      Resource.using(new PrintWriter("src/main/resources/gold/acl/titleNormalized.tsv")) {
        titleNormalizedWriter =>
          Resource.using(new PrintWriter("src/main/resources/gold/acl/venue.tsv")) {
            venueWriter =>
              Resource.using(new PrintWriter("src/main/resources/gold/acl/year.tsv")) {
                yearWriter =>
                  Resource.using(new PrintWriter("src/main/resources/gold/acl/authorFullNameExact.tsv")) {
                    authorFullExactWriter =>
                      Resource.using(new PrintWriter("src/main/resources/gold/acl/authorLastNameExact.tsv")) {
                        authorLastExactWriter =>
                          Resource.using(new PrintWriter("src/main/resources/gold/acl/authorLastNameNormalized.tsv")) {
                            authorLastNormalizedWriter =>
                              Resource.using(Source.fromFile("src/main/resources/raw/acl/raw.json")) {
                                input =>
                                  for {
                                    AclGold(paperId, title, venue, year, authors) <- input.getLines()
                                      .map(_.parseJson.asJsObject.getFields("aclMetadata")(0).convertTo[AclGold]) if (keepId(paperId))
                                  } {
                                    write(paperId, titleExactWriter, title.text)
                                    write(paperId, titleNormalizedWriter, title.text.removePunctuation.normalized)
                                    write(paperId, venueWriter, venue.name)
                                    write(paperId, yearWriter, year.toString)
                                    write(paperId, authorFullExactWriter, authors.map(_.toString): _*)
                                    write(paperId, authorLastExactWriter, authors.map(_.lastName): _*)
                                    write(paperId, authorLastNormalizedWriter, authors.map(_.lastNameNormalized): _*)
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }
}

// Raw format is json from the pipeline, e.g. s3://ai2-s2-pipeline/output/publish/2015-10-24_1100_f6c41/data/WhitelistFilteredPaperClusters.5166da0b815347f6
// Data was created in this pipeline run http://ai2-s2-pipeline.s3.amazonaws.com/output/publish/2015-10-24_1100_f6c41/summary/BuildIndexFromPdfs-2015-11-04-23-53-27.html
object MetadataParser {
  case class ACL(title: Title, venue: Venue, year: Int, authors: List[Author])
  case class DBLP(metadataId: String, authors: List[String], title: Title)
  case class Grobid(title: Title, venue: Venue, year: Int, authors: List[Author])
  case class ScienceParse(title: Title, year: Int, authors: List[Author], highPrecision: Boolean)
  case class Cluster(paperIds: Iterable[String], acl: Option[ACL], dblp: Option[DBLP], grobid: Option[Grobid], scienceParse: Option[ScienceParse])
  implicit val aclFmt = jsonFormat4(ACL.apply)
  implicit val grobidFmt = jsonFormat4(Grobid.apply)
  implicit val dblpFmt = jsonFormat3(DBLP.apply)
  implicit val spFmt = jsonFormat4(ScienceParse.apply)
  def parse(s: String): Cluster = {
    val json = s.parseJson.asJsObject.fields("metadatas").asInstanceOf[JsArray]
    val metas = for (el <- json.elements.map(_.asJsObject)) yield {
      val ids = el.fields("paperIds").convertTo[Array[String]]
      val metadataJson = el.fields("metadata").asJsObject
      val metadata = metadataJson.fields("type").convertTo[String] match {
        case "DBLP" => metadataJson.convertTo[DBLP]
        case "ACL" => metadataJson.convertTo[ACL]
        case "Grobid" => metadataJson.convertTo[Grobid]
        case "ScienceParse" => metadataJson.convertTo[ScienceParse]
        case _ => ()
      }
      (ids, metadata)
    }
    val allIds = metas.flatMap(_._1).toSet.toList
    val acl = metas.map(_._2).collect { case x: ACL => x }.headOption
    val dblp = metas.map(_._2).collect { case x: DBLP => x }.headOption
    val grobid = metas.map(_._2).collect { case x: Grobid => x }.headOption
    val scienceParse = metas.map(_._2).collect { case x: ScienceParse => x }.headOption
    Cluster(allIds, acl, dblp, grobid, scienceParse)
  }
}
