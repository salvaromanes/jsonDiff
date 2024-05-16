package util

import io.circe._
import util.Diff._

object JsonList {

  def differencesBetweenJsonList(
                                  value1: Json,
                                  value2: Json,
                                  head: String,
                                  tail: List[String],
                                  cursor1: HCursor,
                                  cursor2: HCursor
                                )(
                                  jsonSolution: (Json, Json)
                                ): (Json, Json) = {
    val maybeListJson =
      for{
        arrayOrValue1 <- value1.asArray
        arrayOrValue2 <- value2.asArray
      } yield {
        if (shouldContinueSearchingBottomJson(Json.fromValues(arrayOrValue1)) && shouldContinueSearchingBottomJson(Json.fromValues(arrayOrValue2))) {
          diff2Json(Json.fromValues(arrayOrValue1).hcursor, Json.fromValues(arrayOrValue2).hcursor)
        } else {
          (Json.fromValues(arrayOrValue1.diff(arrayOrValue2)),
            Json.fromValues(arrayOrValue2.diff(arrayOrValue1)))
        }
      }

    maybeListJson match {
      case None =>
        solutionMaker(jsonSolution, tail, cursor1, cursor2, JsonKeysValues(head, value1).toJson, JsonKeysValues(head, value2).toJson)
      case Some(value) =>
        solutionMaker(jsonSolution, tail, cursor1, cursor2, JsonKeysValues(head, value._1).toJson, JsonKeysValues(head, value._2).toJson)
    }
  }

}
