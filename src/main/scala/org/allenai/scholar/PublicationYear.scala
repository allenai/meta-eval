package org.allenai.scholar

import org.allenai.scholar.metrics.metadata.yearZero

object PublicationYear {
  def ifDefined(y: java.time.Year) = y match {
    case _ if y == yearZero => None
    case _ => Some(y)
  }
}
