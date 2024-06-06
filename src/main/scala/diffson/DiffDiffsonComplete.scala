package diffson

import diffson.lcs._
import diffson.circe._
import diffson.jsonpatch._
import diffson.jsonpatch.lcsdiff.remembering._
import io.circe._
import io.circe.parser._

import scala.annotation.tailrec

object DiffDiffsonComplete {

  def buildSolutionWithDiffson(
                                text1: String,
                                text2: String
                              ): Json = {
    val maybeJson1 = parse(text1)
    val maybeJson2 = parse(text2)

    (maybeJson1, maybeJson2) match {
      case (Right(v1), Right(v2)) =>
        val differences = diffDiffson(v1, v2)
        val maybeArrayOfDifferences = differences.asArray

        val listOfDifferences =
          maybeArrayOfDifferences match {
            case Some(arrayOfDifferences) =>
              arrayOfDifferences.toList
            case None =>
              List()
          }

        val cursor1 = v1.hcursor
        val keys1 = cursor1.keys.get.toList

        val mapKey = mapKeyWithList(cursor1, keys1)(Map())
        buildSolutionWithCorrectFormat(listOfDifferences, mapKey)(List(), List(), List())
      case _ =>
        Json.fromString("Something went wrong")
    }
  }

  private def mapKeyWithList(
                              cursor: HCursor,
                              keys: List[String]
                            )(
                              outMap: Map[Json, List[Json]]
                            ): Map[Json, List[Json]] = keys match {
    case Nil =>
      outMap
    case head :: tail =>
      val keyValue = cursor.downField(head).as[Json]

      keyValue match {
        case Right(value) if value.isArray =>
          val newOutMap = outMap.concat(Map(Json.fromString(head) -> value.asArray.get.toList))
          mapKeyWithList(cursor, tail)(newOutMap)
        case Right(value) =>
          val newCursor = value.hcursor
          val newKeys = value.hcursor.keys.getOrElse(List()).toList
          val newOutMap = mapKeyWithList(newCursor, newKeys)(outMap)
          mapKeyWithList(cursor, tail)(newOutMap)
        case _ =>
          mapKeyWithList(cursor, tail)(outMap)
      }
  }


  @tailrec
  private def buildSolutionWithCorrectFormat(
                                              differences: List[Json],
                                              mapKeyWithList: Map[Json, List[Json]]
                                            )(
                                              changeList: List[Json],
                                              deleteList: List[Json],
                                              addList: List[Json]
                                            ): Json = differences match {
    case Nil =>
      val changeJson = Json.obj(("change", Json.fromValues(changeList)))
      val removeJson = Json.obj(("remove", Json.fromValues(deleteList)))
      val addJson = Json.obj(("add", Json.fromValues(addList)))

      addJson.deepMerge(removeJson).deepMerge(changeJson)
    case head::tail =>
      val cursor = head.hcursor
      val maybeOperation = cursor.downField("op").as[Json]

      maybeOperation match {
        case Right(operation) =>
          val maybePathField = cursor.downField("path").as[Json]
          val maybeNewField = cursor.downField("value").as[Json]
          val maybeOldField = cursor.downField("old").as[Json]

          val listOfJsonDifferences =
            operation match {
              case op if op.equals(Json.fromString("replace")) =>
                val newElementOfList =
                  for {
                    pathField <- maybePathField
                    newField <- maybeNewField
                    oldField <- maybeOldField
                  } yield {
                    val keyRegexForAllKeyWithSeparator = "[a-zA-Z]+/".r
                    val keyRegexForKeys = "[a-zA-Z]+".r
                    val allKeysWithSeparator = keyRegexForAllKeyWithSeparator.findAllIn(pathField.toString()).mkString
                    val keyList = keyRegexForKeys.findAllIn(allKeysWithSeparator).toList

                    val (keyJson, value) =
                      keyList match {
                        case kl if kl.isEmpty =>
                          val regexForKeyJson = keyRegexForKeys.findAllIn(pathField.toString()).mkString
                          (Json.fromString(regexForKeyJson), oldField)
                        case _ =>
                          (Json.fromString(keyList.last), oldField)
                      }

                    if (mapKeyWithList.contains(keyJson)) {
                      val index = mapKeyWithList(keyJson).indexOf(value)

                      Json.obj(
                        ("path", Json.fromString(s"${keyList.mkString(".")}.[$index]")),
                        ("new", newField),
                        ("old", oldField)
                      )
                    } else {
                      val pathString = pathField.toString()
                      val jsonPath = Json.fromString(pathString.substring(2, pathString.length - 1).replace("/", "."))

                      Json.obj(
                        ("path", jsonPath),
                        ("new", newField),
                        ("old", oldField)
                      )
                    }
                  }

                newElementOfList match {
                  case Right(value) =>
                    (changeList ++ List(value), deleteList, addList)
                }
              case op if op.equals(Json.fromString("remove")) =>
                val newElementOfList =
                  for {
                    pathField <- maybePathField
                    oldField <- maybeOldField
                  } yield {
                    val keyRegexForAllKeyWithSeparator = "[a-zA-Z]+/".r
                    val keyRegexForKeys = "[a-zA-Z]+".r
                    val allKeysWithSeparator = keyRegexForAllKeyWithSeparator.findAllIn(pathField.toString()).mkString
                    val keyList = keyRegexForKeys.findAllIn(allKeysWithSeparator).toList

                    val (keyJson, value) =
                      keyList match {
                        case kl if kl.isEmpty =>
                          val regexForKeyJson = keyRegexForKeys.findAllIn(pathField.toString()).mkString
                          (Json.fromString(regexForKeyJson), oldField)
                        case _ =>
                          (Json.fromString(keyList.last), oldField)
                      }

                    if (mapKeyWithList.contains(keyJson)) {
                      val index = mapKeyWithList(keyJson).indexOf(value)

                      Json.obj(
                        ("path", Json.fromString(s"${keyList.mkString(".")}.[$index]")),
                        ("old", oldField)
                      )
                    } else {
                      val pathString = pathField.toString()
                      val jsonPath = Json.fromString(pathString.substring(2, pathString.length - 1).replace("/", "."))

                      Json.obj(
                        ("path", jsonPath),
                        ("old", oldField)
                      )
                    }
                  }

                newElementOfList match {
                  case Right(value) =>
                    (changeList, deleteList ++ List(value), addList)
                }
              case op if op.equals(Json.fromString("add")) =>
                val newElementOfList =
                  for {
                    pathField <- maybePathField
                    newField <- maybeNewField
                  } yield {
                    val keyRegexForAllKeyWithSeparator = "[a-zA-Z]+/".r
                    val keyRegexForKeys = "[a-zA-Z]+".r
                    val allKeysWithSeparator = keyRegexForAllKeyWithSeparator.findAllIn(pathField.toString()).mkString
                    val keyList = keyRegexForKeys.findAllIn(allKeysWithSeparator).toList

                    val (keyJson, value) =
                      keyList match {
                        case kl if kl.isEmpty =>
                          val regexForKeyJson = keyRegexForKeys.findAllIn(pathField.toString()).mkString
                          (Json.fromString(regexForKeyJson), newField)
                        case _ =>
                          (Json.fromString(keyList.last), newField)
                      }

                    if (mapKeyWithList.contains(keyJson)) {
                      val index =
                        mapKeyWithList(keyJson).indexOf(value) match {
                          case i if i == -1 =>
                            mapKeyWithList(keyJson).length
                          case i =>
                            i
                        }

                      Json.obj(
                        ("path", Json.fromString(s"${keyList.mkString(".")}.[$index]")),
                        ("new", newField)
                      )
                    } else {
                      val pathString = pathField.toString()
                      val jsonPath = Json.fromString(pathString.substring(2, pathString.length - 1).replace("/", "."))

                      Json.obj(
                        ("path", jsonPath),
                        ("new", newField)
                      )
                    }
                  }

                newElementOfList match {
                  case Right(value) =>
                    (changeList, deleteList, addList ++ List(value))
                }
            }

          buildSolutionWithCorrectFormat(tail, mapKeyWithList)(listOfJsonDifferences._1, listOfJsonDifferences._2, listOfJsonDifferences._3)
        case Left(error) =>
          Json.fromString(error.getMessage())
      }

  }

  private def diffDiffson(
                           json1: Json,
                           json2: Json
                         ): Json = {
    implicit val lcs: Patience[Json] = new Patience[Json]
    val encoder = Encoder[JsonPatch[Json]]
    encoder(diff(json1, json2))
  }

}
