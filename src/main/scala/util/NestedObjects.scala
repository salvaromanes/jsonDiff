package util

import io.circe._
import util.Diff._

object NestedObjects {

  def differencesBetweenNestedObjects(
                                  value1: Json,
                                  value2: Json,
                                  tail: List[String],
                                  cursor1: HCursor,
                                  cursor2: HCursor
                                )(
                                  jsonSolution: (Json, Json)
                                ): (Json, Json) = {
    val maybeSolution = {
      val c1 = value1.hcursor
      val c2 = value2.hcursor

      for{
        keys1 <- c1.keys
        keys2 <- c2.keys
      } yield {
        val allKeysSet = (keys1 ++ keys2).toSet
        createSolution(allKeysSet.toList, c1, c2)(Json.Null, Json.Null)
      }
    }

    maybeSolution match {
      case None =>
        createSolution(tail, cursor1, cursor2)(jsonSolution)
      case Some(value) =>
        createSolution(tail, cursor1, cursor2)(value._1, value._2)
    }
  }

}
