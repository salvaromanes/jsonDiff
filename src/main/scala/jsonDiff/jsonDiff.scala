package jsonDiff

import diffson.DiffDiffson.buildSolutionWithDiffson
import io.circe._
import io.circe.parser._

import scala.annotation.tailrec

object jsonDiff {

  def buildJsonDiffSolution(
                      text1: String,
                      text2: String
                   ): Json = {
    val maybeDiff = buildSolutionWithDiffson(text1, text2)

    maybeDiff match {
      case Right(differences) =>
        val maybeOriginalJson = parse(text1)

        maybeOriginalJson match {
          case Right(original) =>
            diffTwoJson(original, differences)
          case _ =>
            Json.fromString("Something went wrong")
        }
      case Left(error) =>
        error
    }
  }

  private def diffTwoJson(
                           original: Json,
                           differences: Json
                         ): Json = {
    val c1 = original.hcursor
    val c2 = differences.hcursor

    val maybeKeys1 = c1.keys.map(k => k.toList)
    val maybeKeys2 = c2.keys.map(k => k.toList)

    val maybeSolution =
      for {
        keys1 <- maybeKeys1
        keys2 <- maybeKeys2
      } yield {
        val allKeys = keys1.concat(keys2).toSet
        buildJsonSolution(c1, c2, keys1, keys2, allKeys.toList)(Json.Null)
      }

    maybeSolution match {
      case None =>
        Json.fromString("Something went wrong")
      case Some(value) =>
        value
    }
  }

  @tailrec
  private def buildJsonSolution(
                                  originalCursor: HCursor,
                                  differencesCursor: HCursor,
                                  originalKeys: List[String],
                                  differencesKeys: List[String],
                                  keys: List[String],
                               )(
                                  jsonSolution: Json
                                ): Json = keys match {
    case Nil =>
      jsonSolution
    case head::tail =>
      if (differencesKeys.contains(head)) {
        val originalValue = originalCursor.downField(head).as[Json]
        val differentValue = differencesCursor.downField(head).as[Json]

        val originalJson =
          originalValue match {
            case Right(v) =>
              Json.obj(("_old", v))
//              Json.obj(("oldValue", Json.fromString(Console.RED + v)))
            case _ =>
              Json.Null
          }

        val differentJson =
          differentValue match {
            case Right(v) =>
              Json.obj(("_new", v))
//              Json.obj(("newValue", Json.fromString(Console.GREEN + v)))
            case _ =>
              Json.Null
          }

        val json = Json.obj((head, differentJson.deepMerge(originalJson)))
        val outJson =
          if (jsonSolution.isNull) json
          else json.deepMerge(jsonSolution)

        buildJsonSolution(
          originalCursor, differencesCursor, originalKeys, differencesKeys, tail
        )(
          outJson
        )
      } else {
        val value = originalCursor.downField(head).as[Json]

        val json =
          value match {
            case Right(v) =>
              Json.obj((head, v))
            case _ =>
              Json.Null
          }

        val outJson =
          if (jsonSolution.isNull) json
          else json.deepMerge(jsonSolution)

        buildJsonSolution(
          originalCursor, differencesCursor, originalKeys, differencesKeys, tail
        )(
          outJson
        )
      }
  }

}
