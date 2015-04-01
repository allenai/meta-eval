package org.allenai.scholar

import StringUtils._
import spray.json.DefaultJsonProtocol._

case class Author(firstName: String, lastName: String, middleNames: Seq[String]) {
  def normalized = Author(firstName.normalize, lastName.normalize, middleNames.map(_.normalize))

  def lastNameOnly = Author("", lastName, Seq())

  def lastNameFirstInitial = Author(firstName.take(1), lastName, Seq())
}

object Author {
  def commmaSeparated(s: String) = {
    val i = s.indexOf(',')
    i match {
      case -1 =>
        val names = s.splitOnWhitespace()
        Author("", names.head, names.tail)
      case _ =>
        val firstName = s.substring(0,i)
        val names = s.substring(i).splitOnWhitespace()
        Author(firstName, names.head, names.tail)
    }
  }
  implicit val JsFormat = jsonFormat3(Author.apply)
}
