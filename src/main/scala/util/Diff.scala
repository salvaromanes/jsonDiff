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
        if(value._1.isNull && value._2.isNull)
          Json.obj(("Differences", Json.fromString("No differences")))
        else
          JsonKeysValues("Differences", Json.arr(value._1, value._2)).toJson
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
  private def createSolution(
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
          val maybeList = {
            for{
              maybeList1 <- value1.asArray
              maybeList2 <- value2.asArray
            } yield {
              differencesBetweenJsonList(maybeList1, maybeList2)
            }
          }

          maybeList match {
            case Some(value) =>
              val json1 = JsonKeysValues(head, value._1).toJson
              val json2 = JsonKeysValues(head, value._2).toJson

              solutionMaker(jsonSolution, tail, cursor1, cursor2, json1, json2)
            case None =>
              val nestedDifferences = differencesBetweenNestedObjects(value1, value2)
              val json1 = JsonKeysValues(head, nestedDifferences._1).toJson
              val json2 = JsonKeysValues(head, nestedDifferences._2).toJson

              solutionMaker(jsonSolution, tail, cursor1, cursor2, json1, json2)
          }
        case (Right(value1), Left(_)) =>
          val json1 = JsonKeysValues(head, value1).toJson
          val json2 = JsonKeysValues(head, Json.Null).toJson

          solutionMaker(jsonSolution, tail, cursor1, cursor2, json1, json2)
        case (Left(_), Right(value2)) =>
          val json1 = JsonKeysValues(head, Json.Null).toJson
          val json2 = JsonKeysValues(head, value2).toJson

          solutionMaker(jsonSolution, tail, cursor1, cursor2, json1, json2)
        case _ =>
          createSolution(tail, cursor1, cursor2)(jsonSolution)
      }
  }

  private def solutionMaker(
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
      val jsonSolution1 = json1.deepMerge(jsonSolution._1)
      val jsonSolution2 = json2.deepMerge(jsonSolution._2)

      createSolution(tail, cursor1, cursor2)(jsonSolution1, jsonSolution2)
    }
  }

}
