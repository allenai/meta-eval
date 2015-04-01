package org.allenai.scholar

import spray.json.DefaultJsonProtocol._

case class Venue(name: String) {
  def nonEmpty = name match {
    case "" => Venue("")
    case _ => Venue("nonEmpty")
  }
}

object Venue {
  implicit val JsFormat = jsonFormat1(Venue.apply)
}
