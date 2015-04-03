package org.allenai.scholar

import StringUtils._
import spray.json.DefaultJsonProtocol._

case class Author(firstName: String, middleNames: Seq[String], lastName: String) {
  def normalized = Author(firstName.normalize, middleNames.map(_.normalize), lastName.normalize)

  def lastNameOnly = Author("", Seq(), lastName)

  def lastNameFirstInitial = Author(firstName.take(1), Seq(), lastName)

  def ifDefined =
    if (firstName.isEmpty
      && middleNames.isEmpty
      && lastName.isEmpty) {
      None
    } else {
      Some(this)
    }
}

object Author {
  // If string contains a comma, read as "last, first middle"
  // If no comma, read as "first middle last"
  def parse(s: String) = {
    val i = s.indexOf(',')
    i match {
      case -1 =>
        val names = s.removePunctuation.splitOnWhitespace
        names.size match {
          case 1 => Author("", Seq(), names(0))
          case _ => Author(names.head, names.drop(1).take(names.size - 2), names.last)
        }
      case _ =>
        val lastName = s.substring(0, i).removePunctuation.trim
        val names = s.substring(i + 1).removePunctuation.trim.splitOnWhitespace()
        Author(names.head, names.tail, lastName)
    }
  }

  implicit val JsFormat = jsonFormat3(Author.apply)
}
