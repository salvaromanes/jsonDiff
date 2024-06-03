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

      mapKeyWithListOfValues(diffKeyList, diffCursor)(Map.empty).foreach(m =>
        println(s"${m._1} : ${m._2.mkString(Console.WHITE + ",")}")
      )
    }
  }

  private def printDifferencesByKeys(
                                      diffKeyList: List[String],
                                      diffCursor: HCursor
                                    ): Unit = {
    diffKeyList.foreach{key =>
      val value = diffCursor.downField(key).as[Json]
      val valueString = s"${value.getOrElse("empty").toString}"
      val keyString = s"${key.substring(0, key.length - 4)} : "

      print(Console.WHITE + keyString)
      key match {
        case k if k.endsWith("_old") =>
          println(Console.RED + valueString)
        case k if k.endsWith("_new") =>
          println(Console.GREEN + valueString)
        case _ =>
          println(Console.WHITE + valueString)
      }
    }
  }

  @tailrec
  private def mapKeyWithListOfValues(
                                      keysList: List[String],
                                      cursor: HCursor
                                    )(
                                      outputMap: Map[String, List[String]]
                                    ): Map[String, List[String]] = keysList match {
    case Nil =>
      outputMap
    case head::_ =>
      head match {
        case k if k.endsWith("_new") || k.endsWith("_old") =>
          val key = head.substring(0, head.length - 4)
          val newValue = cursor.downField(s"${key}_new").as[Json]
          val oldValue = cursor.downField(s"${key}_old").as[Json]

          val keyString = Console.WHITE + key
          val listOffValues = List(Console.GREEN + newValue.getOrElse("empty"), Console.RED + oldValue.getOrElse("empty"))

          val index = keysList.indexOf(s"${key}_old") + 1
          val newKeyList = keysList.drop(index)

          val newOutputMap =
            outputMap match {
              case outMap if outMap.isEmpty =>
                Map(keyString -> listOffValues)
              case _ =>
                outputMap.concat(Map(keyString -> listOffValues))
            }

          mapKeyWithListOfValues(newKeyList, cursor)(newOutputMap)
        case _ =>
          val keyString = Console.WHITE + head.substring(0, head.length - 4)
          val eqValue = cursor.downField(head).as[Json]
          val listOffValues = List(Console.WHITE + eqValue.getOrElse("empty"))

          val index = keysList.indexOf(s"$head") + 1
          val newKeyList = keysList.drop(index)

          val newOutputMap =
            outputMap match {
              case outMap if outMap.isEmpty =>
                Map(keyString -> listOffValues)
              case _ =>
                outputMap.concat(Map(keyString -> listOffValues))
            }

          mapKeyWithListOfValues(newKeyList, cursor)(newOutputMap)
      }
  }

  def buildJsonDiffSolution(
                      text1: String,
                      text2: String
                   ): Json = {
    val maybeDiff = (buildSolutionWithDiffson(text1, text2), buildSolutionWithDiffson(text2, text1))

    maybeDiff match {
      case (Right(differencesOldToNew), Right(differencesNewToOld)) =>
        val maybeOriginalJson = parse(text1)

        maybeOriginalJson match {
          case Right(original) =>
            diffTwoJson(original, differencesOldToNew, differencesNewToOld)
          case Left(error) =>
            Json.fromString(s"Something went wrong trying to get the differencesOldToNew. Error: $error")
        }
      case _ =>
        throw new RuntimeException("Error in buildJsonDiffSolution: Left has been returning")
    }
  }

  private def diffTwoJson(
                           original: Json,
                           differencesOldToNew: Json,
                           differencesNewToOld: Json
                         ): Json = {
    val originalCursor = original.hcursor
    val differencesOldToNewCursor = differencesOldToNew.hcursor
    val differencesNewToOldCursor = differencesNewToOld.hcursor

    val maybeSolution =
      for {
        keys1 <- originalCursor.keys.map(k => k.toList)
        keys2 <- differencesOldToNewCursor.keys.map(k => k.toList)
        keys3 <- differencesNewToOldCursor.keys.map(k => k.toList)
      } yield {
        val allKeys = keys1.concat(keys2).concat(keys3).toSet
        buildJsonSolution(originalCursor, differencesOldToNewCursor, differencesNewToOldCursor, keys1, keys2, allKeys.toList)(Json.Null)
      }

    maybeSolution match {
      case Some(value) =>
        value
    }
  }

  @tailrec
  private def buildJsonSolution(
                                 originalCursor: HCursor,
                                 differencesOldToNewCursor: HCursor,
                                 differencesNewToOldCursor: HCursor,
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
        val differentOldToNewValue = differencesOldToNewCursor.downField(head).as[Json]
        val differentNewToOldValue = differencesNewToOldCursor.downField(head).as[Json]

        val values =
          (differentNewToOldValue, differentOldToNewValue) match {
            case (Right(v1), Right(v2)) =>
              Json.obj((s"${head}_old", v1)).deepMerge(Json.obj((s"${head}_new", v2)))
            case _ =>
              Json.Null
          }

        val outJson =
          if (jsonSolution.isNull) values
          else values.deepMerge(jsonSolution)

        buildJsonSolution(
          originalCursor, differencesOldToNewCursor, differencesNewToOldCursor, originalKeys, differencesKeys, tail
        )(outJson)
      } else {
        val value = originalCursor.downField(head).as[Json]

        val json =
          value match {
            case Right(v) =>
              Json.obj((s"${head}__eq", v))
            case _ =>
              Json.Null
          }

        val outJson =
          if (jsonSolution.isNull)
            json
          else
            json.deepMerge(jsonSolution)

        buildJsonSolution(
          originalCursor, differencesOldToNewCursor, differencesNewToOldCursor, originalKeys, differencesKeys, tail
        )(outJson)
      }
  }

}
