package org.allenai.scholar

import StringUtils._
import spray.json.DefaultJsonProtocol._

case class Title(text: String) {
  def normalized = Title(text.removePunctuation().normalize())

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