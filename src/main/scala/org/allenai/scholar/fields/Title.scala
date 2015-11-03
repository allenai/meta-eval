package org.allenai.scholar.fields

import spray.json.DefaultJsonProtocol._
import org.allenai.scholar.StringUtils._

case class Title(text: String) {
  def normalized = Title(text.removePunctuation.normalized)

  def nonEmpty =
    text match {
      case "" => this
      case _ => Title("nonEmpty")
    }

  def ifDefined = text match {
    case "" => None
    case _ => Some(this)
  }
}

object Title {
  implicit val JsFormat = jsonFormat1(Title.apply)
}