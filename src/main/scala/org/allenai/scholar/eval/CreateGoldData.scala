package org.allenai.scholar.eval

import java.io.{ File, PrintWriter }

import org.allenai.common.Resource
import org.allenai.scholar.StringUtils._
import org.allenai.scholar.fields.{ Author, Title, Venue }
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

/** Run this to create .tsv files with different fields extracted based on a raw data file, whose format depends on the source
  */
object CreateGoldData extends App {
  createAclGold()
  createGrobidAclBaseline()
  createScienceParseAclBaseline()
  createDblpGoldAndBaseline()

  def createDblpGoldAndBaseline() {
    import MetadataParser._
    val rand = new Random()
    var size = 1000.0
    var count = 1.0
    val clusters = new ListBuffer[Cluster]
    val keepId: String => Boolean = {
      val idFile = new File("src/main/resources/raw/dblp/ids.txt")
      if (idFile.exists) // Use ID file if it exists
      {
        val ids = Resource.using(Source.fromFile(idFile)) { src => src.getLines.toSet }
        s: String => ids(s)
      } else // Resevoir sample 1000 papers
      {
        s: String => (rand.nextDouble < size / count)
      }
    }
    Resource.using(Source.fromFile(pipelineMetadataOutputFile)) { src =>
      for {
        c @ Cluster(ids, acl, Some(dblp), grobid, scienceParse) <- src.getLines().map(parse) if keepId(ids.head)
      } {
        if (count <= size) {
          clusters += c
        } else {
          clusters(rand.nextInt(clusters.size)) = c
        }
        count += 1
      }
    }
    Resource.using(new PrintWriter("src/main/resources/baseline/dblp/scienceParse/titleExact.tsv")) { spTitleExact =>
      Resource.using(new PrintWriter("src/main/resources/baseline/dblp/scienceParse/titleNormalized.tsv")) { spTitleNorm =>
        Resource.using(new PrintWriter("src/main/resources/baseline/dblp/scienceParse-highP/titleExact.tsv")) { sphpTitleExact =>
          Resource.using(new PrintWriter("src/main/resources/baseline/dblp/scienceParse-highP/titleNormalized.tsv")) { sphpTitleNorm =>
            Resource.using(new PrintWriter("src/main/resources/baseline/dblp/grobid/titleExact.tsv")) { gTitleExact =>
              Resource.using(new PrintWriter("src/main/resources/baseline/dblp/grobid/titleNormalized.tsv")) { gTitleNorm =>
                Resource.using(new PrintWriter("src/main/resources/baseline/dblp/grobid/authorFullNameExact.tsv")) { gAuthorFull =>
                  Resource.using(new PrintWriter("src/main/resources/baseline/dblp/grobid/authorLastNameNormalized.tsv")) { gAuthorLastNorm =>
                    Resource.using(new PrintWriter("src/main/resources/baseline/dblp/grobid/authorLastNameExact.tsv")) { gAuthorLast =>
                      Resource.using(new PrintWriter("src/main/resources/gold/dblp/titleExact.tsv")) { goldTitle =>
                        Resource.using(new PrintWriter("src/main/resources/gold/dblp/titleNormalized.tsv")) { goldTitleNorm =>
                          Resource.using(new PrintWriter("src/main/resources/gold/dblp/authorFullNameExact.tsv")) { goldAuthor =>
                            Resource.using(new PrintWriter("src/main/resources/gold/dblp/authorLastNameExact.tsv")) { goldAuthorLast =>
                              Resource.using(new PrintWriter("src/main/resources/gold/dblp/authorLastNameNormalized.tsv")) { goldAuthorLastNorm =>
                                for {
                                  Cluster(ids, acl, Some(dblp), grobid, scienceParse) <- clusters
                                  paperId = ids.head
                                } {
                                  count += 1
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

  def write(paperId: String, w: PrintWriter, data: String*) = {
    val nonEmptyData = data.filter(_.length > 0)
    if (nonEmptyData.nonEmpty) {
      w.println(s"$paperId\t${nonEmptyData.mkString("\t")}")
    }
  }

  // Downloaded from http://ai2-s2-pipeline.s3.amazonaws.com/output/publish/2015-10-24_1100_f6c41/data/GoldDataForMetaEval.527cc65fdde19cc8.json
  def pipelineMetadataOutputFile = "/Users/rodneykinney/Downloads/GoldDataForMetaEval.527cc65fdde19cc8.json"

  def createScienceParseAclBaseline(): Unit = {
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
  def createGrobidAclBaseline(): Unit = {
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
  def createAclGold(): Unit = {
    val keepId = Source.fromFile("src/main/resources/raw/acl/ids.txt").getLines().toSet
    case class AclGold(paperId: String, title: Title, venue: Venue, year: Int, authors: List[Author])
    implicit val titleFmt = jsonFormat1(Title.apply)
    implicit val venueFmt = jsonFormat1(Venue.apply)
    implicit val authorFms = jsonFormat3(Author.apply)
    implicit val fmt = jsonFormat5(AclGold)
    Resource.using(new PrintWriter("src/main/resources/gold/acl/titleExact.tsv")) { titleExactWriter =>
      Resource.using(new PrintWriter("src/main/resources/gold/acl/titleNormalized.tsv")) { titleNormalizedWriter =>
        Resource.using(new PrintWriter("src/main/resources/gold/acl/venue.tsv")) { venueWriter =>
          Resource.using(new PrintWriter("src/main/resources/gold/acl/year.tsv")) { yearWriter =>
            Resource.using(new PrintWriter("src/main/resources/gold/acl/authorFullNameExact.tsv")) { authorFullExactWriter =>
              Resource.using(new PrintWriter("src/main/resources/gold/acl/authorLastNameExact.tsv")) { authorLastExactWriter =>
                Resource.using(new PrintWriter("src/main/resources/gold/acl/authorLastNameNormalized.tsv")) { authorLastNormalizedWriter =>
                  Resource.using(Source.fromFile("src/main/resources/raw/acl/raw.json")) { input =>
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
