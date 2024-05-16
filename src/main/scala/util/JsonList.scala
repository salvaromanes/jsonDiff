package util

import io.circe._
import util.Diff._

object JsonList {

  //It doesn't work correctly if there are nested objects inside the list

  def differencesBetweenJsonList(
                                  jsonList1: Json,
                                  jsonList2: Json,
                                ): (Json, Json) = {
    val cursor1 = jsonList1.hcursor
    val cursor2 = jsonList2.hcursor

    val keys1 = cursor1.keys
    val keys2 = cursor2.keys

    (keys1, keys2) match {
      case (None, None) =>
        val maybeList =
          for {
            list1 <- jsonList1.asArray
            list2 <- jsonList2.asArray
          } yield {
            (Json.fromValues(list1.diff(list2)), Json.fromValues(list2.diff(list1)))
          }

        maybeList match {
          case None =>
            (jsonList1, jsonList2)
          case Some(value) =>
            value
        }
      case (Some(_), Some(_)) =>
        diff2Json(cursor1, cursor2)
      case (Some(_), None) =>
        (jsonList1, jsonList2)
      case (None, Some(_)) =>
        (jsonList1, jsonList2)
    }
  }

//  private def isNestedObject(json: Json): Boolean = {
//    val keys = json.hcursor.keys
//
//    keys match {
//      case None => false
//      case Some(_) => true
//    }
//  }

}
