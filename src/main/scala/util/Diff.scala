package util

import io.circe._
import io.circe.parser._
import util.JsonList._
import util.NestedObjects._

import scala.annotation.tailrec

object Diff {

  def diff(
      entry1: String,
      entry2: String
          ): Json = {
    val outputJson =
      for{
        entryJson1 <- parse(entry1)
        entryJson2 <- parse(entry2)
      } yield {
        if (entryJson1.equals(entryJson2)) (Json.Null, Json.Null)
        else diff2Json(entryJson1.hcursor, entryJson2.hcursor)
      }

    outputJson match {
      case Left(error) => Json.fromString(error.getMessage())
      case Right(value) =>
        if(value._1.isNull && value._2.isNull) Json.obj(("Differences", Json.fromString("No differences")))
        else JsonKeysValues("Differences", Json.arr(value._1, value._2)).toJson
    }
  }

  def diff2Json(
                  cursor1: HCursor,
                  cursor2: HCursor
               ): (Json, Json) = {
    val differences =
      for{
        keys1 <- cursor1.keys
        keys2 <- cursor2.keys
      } yield {
        val allKeysSet = (keys1 ++ keys2).toSet
        createSolution(allKeysSet.toList, cursor1, cursor2)(Json.Null, Json.Null)
      }

    differences match {
      case None => (Json.Null, Json.Null)
      case Some(value) => value
    }
  }

  @tailrec
  def createSolution(
                      keys: List[String],
                      cursor1: HCursor,
                      cursor2: HCursor
                    )(
                      jsonSolution: (Json, Json)
                    ): (Json, Json) = keys match {
    case Nil => jsonSolution
    case head::tail =>
      val data1 = cursor1.downField(head).as[Json]
      val data2 = cursor2.downField(head).as[Json]

      (data1, data2) match {
        case (Right(value1), Right(value2)) if value1 != value2 =>
          if (shouldContinueSearchingBottomJson(value1) && shouldContinueSearchingBottomJson(value2))
            differencesBetweenNestedObjects(value1, value2, tail, cursor1, cursor2)(jsonSolution)
          else
            differencesBetweenJsonList(value1, value2, head, tail, cursor1, cursor2)(jsonSolution)
        case (Right(value1), _) =>
          solutionMaker(jsonSolution, tail, cursor1, cursor2, JsonKeysValues(head, value1).toJson, JsonKeysValues(head, Json.Null).toJson)
        case (_, Right(value2)) =>
          solutionMaker(jsonSolution, tail, cursor1, cursor2, JsonKeysValues(head, Json.Null).toJson, JsonKeysValues(head, value2).toJson)
        case _ =>
          createSolution(tail, cursor1, cursor2)(jsonSolution)
      }
  }

  def shouldContinueSearchingBottomJson(
                           value: Json
                         ): Boolean = {
    value.asString match {
      case None => false
      case Some(value) => value.contains("{") && value.contains("}")
    }
  }

  def solutionMaker(
                             jsonSolution: (Json, Json),
                             tail: List[String],
                             cursor1: HCursor,
                             cursor2: HCursor,
                             json1: Json,
                             json2: Json
                           ): (Json, Json) = {
    if (jsonSolution._1.isNull && jsonSolution._2.isNull) {
      createSolution(tail, cursor1, cursor2)(json1, json2)
    } else {
      createSolution(tail, cursor1, cursor2)(Json.arr(jsonSolution._1, json1), Json.arr(jsonSolution._2, json2))
    }
  }

}
