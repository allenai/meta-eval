package org.allenai.scholar.fields

import org.allenai.scholar.StringUtils._

import spray.json.DefaultJsonProtocol._

case class Venue(name: String) {
  def normalized: Venue = copy(name = name.normalized)

  def nonEmpty = name match {
    case "" => this
    case _ => Venue("nonEmpty")
  }

  def ifDefined = name match {
    case "" => None
    case _ => Some(this)
  }
}

object Venue {
  implicit val JsFormat = jsonFormat1(Venue.apply)
}
