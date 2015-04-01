package org.allenai.scholar

import spray.json.DefaultJsonProtocol._

case class Venue(name: String)

object Venue {
  implicit val JsFormat = jsonFormat1(Venue.apply)
}
