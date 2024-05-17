package util

import io.circe._
import util.Diff._

object NestedObjects {

  def differencesBetweenNestedObjects(
                                  value1: Json,
                                  value2: Json
                                ): (Json, Json) = {
    val cursor1 = value1.hcursor
    val cursor2 = value2.hcursor

    val keys1 = cursor1.keys
    val keys2 = cursor2.keys

    (keys1, keys2) match {
      case (None, None) =>
        (value1, value2)
      case (Some(_), Some(_)) =>
        diff2Json(cursor1, cursor2)
      case (Some(_), None) =>
        (value1, value2)
      case (None, Some(_)) =>
        (value1, value2)
    }
  }

}
