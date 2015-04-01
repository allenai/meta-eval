package org.allenai.scholar

import StringUtils._
import spray.json.DefaultJsonProtocol._

case class Title(text: String) {
  def normalized = Title.normalized(text)

  def nonEmpty =
    text match {
      case "" => Title(text)
      case _ => Title("nonEmpty")
    }
}

object Title {
  def exact(s: String) = Title(s)

  def normalized(s: String) = Title(s.normalize)

  implicit val JsFormat = jsonFormat1(Title.apply)
}