package jsonDiff

import diffson.DiffDiffson.buildSolutionWithDiffson
import io.circe._
import io.circe.parser._

import scala.annotation.tailrec

object jsonDiff {

  def printDiffTwoJsonWithColor(
                        text1: String,
                        text2: String
                      ): Unit = {
    val differencesJson = buildJsonDiffSolution(text1, text2)
    val diffCursor = differencesJson.hcursor

    val maybeDiffKeys = diffCursor.keys

    for {
      diffKeys <- maybeDiffKeys
    } yield {
      val diffKeyList = diffKeys.toList

      diffKeyList.foreach{key =>
        val value = diffCursor.downField(key).as[Json]
        val content = s"${key.substring(0, key.length - 4)} : ${value.getOrElse("empty").toString}"

        key match {
          case k if k.endsWith("_old") =>
            println(Console.RED + content)
          case k if k.endsWith("_new") =>
            println(Console.GREEN + content)
          case _ =>
            println(Console.WHITE + content)
        }
      }
    }
  }

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

        val values =
          (originalValue, differentValue) match {
            case (Right(v1), Right(v2)) =>
              Json.obj((s"${head}_old", v1)).deepMerge(Json.obj((s"${head}_new", v2)))
//              Json.obj(
//                (head, Json.fromString(Console.RED + v1)),
//                (head, Json.fromString(Console.GREEN + v2))
//              )
            case _ =>
              Json.Null
          }

        val outJson =
          if (jsonSolution.isNull) values
          else values.deepMerge(jsonSolution)

        buildJsonSolution(
          originalCursor, differencesCursor, originalKeys, differencesKeys, tail
        )(outJson)
      } else {
        val value = originalCursor.downField(head).as[Json]

        val json =
          value match {
            case Right(v) =>
              Json.obj((s"${head}__eq", v))
//              Json.obj((head, Json.fromString(Console.WHITE + v)))
            case _ =>
              Json.Null
          }

        val outJson =
          if (jsonSolution.isNull)
            json
          else
            json.deepMerge(jsonSolution)

        buildJsonSolution(
          originalCursor, differencesCursor, originalKeys, differencesKeys, tail
        )(outJson)
      }
  }

}
