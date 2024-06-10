package old.diffson

import io.circe.Json

import scala.annotation.tailrec

object FormatBuilder {

  private val regexForKeysWithSeparator = "[a-zA-Z]+/".r
  private val regexForKeys = "[a-zA-Z]+".r

  @tailrec
  def buildSolutionWithCorrectFormat(
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
    case head :: tail =>
      val cursor = head.hcursor
      val maybeOperation = cursor.downField("op").as[Json]

      val maybePathField = cursor.downField("path").as[Json]
      val maybeNewField = cursor.downField("value").as[Json]
      val maybeOldField = cursor.downField("old").as[Json]

      val pathField = maybePathField.getOrElse(Json.Null)
      val newField = maybeNewField.getOrElse(Json.Null)
      val oldField = maybeOldField.getOrElse(Json.Null)

      val jsonWithTheDifferences =
        maybeOperation match {
          case Right(operation) =>
            operation match {
              case op if op.equals(Json.fromString("replace")) =>
                makeSolutionForReplaceOperation(
                  pathField,
                  newField,
                  oldField,
                  mapKeyWithList
                )(changeList, deleteList, addList)
              case op if op.equals(Json.fromString("remove")) =>
                makeSolutionForRemoveOperation(
                  pathField,
                  oldField,
                  mapKeyWithList
                )(changeList, deleteList, addList)
              case op if op.equals(Json.fromString("add")) =>
                makeSolutionForAddOperation(
                  pathField,
                  newField,
                  mapKeyWithList
                )(changeList, deleteList, addList)
            }
        }

      buildSolutionWithCorrectFormat(tail, mapKeyWithList)(
        jsonWithTheDifferences._1,
        jsonWithTheDifferences._2,
        jsonWithTheDifferences._3
      )
  }

  private def makeSolutionForReplaceOperation(
      pathField: Json,
      newField: Json,
      oldField: Json,
      mapKeyWithList: Map[Json, List[Json]]
  )(
      changeList: List[Json],
      deleteList: List[Json],
      addList: List[Json]
  ): (List[Json], List[Json], List[Json]) = {
    val keysWithSeparator =
      regexForKeysWithSeparator.findAllIn(pathField.toString()).mkString
    val keyList = regexForKeys.findAllIn(keysWithSeparator).toList

    val (keyJson, value) =
      keyList match {
        case keyList if keyList.isEmpty =>
          val regexForKeyJson =
            regexForKeys.findAllIn(pathField.toString()).mkString
          (Json.fromString(regexForKeyJson), oldField)
        case _ =>
          (Json.fromString(keyList.last), oldField)
      }

    val newElementForTheList =
      if (mapKeyWithList.contains(keyJson)) {
        val index = mapKeyWithList(keyJson).indexOf(value)

        Json.obj(
          ("path", Json.fromString(s"${keyList.mkString(".")}.[$index]")),
          ("new", newField),
          ("old", oldField)
        )
      } else {
        val pathString = pathField.toString()
        val jsonPath = Json.fromString(
          pathString.substring(2, pathString.length - 1).replace("/", ".")
        )

        Json.obj(
          ("path", jsonPath),
          ("new", newField),
          ("old", oldField)
        )
      }

    (changeList ++ List(newElementForTheList), deleteList, addList)
  }

  private def makeSolutionForRemoveOperation(
      pathField: Json,
      oldField: Json,
      mapKeyWithList: Map[Json, List[Json]]
  )(
      changeList: List[Json],
      deleteList: List[Json],
      addList: List[Json]
  ): (List[Json], List[Json], List[Json]) = {
    val keysWithSeparator =
      regexForKeysWithSeparator.findAllIn(pathField.toString()).mkString
    val keyList = regexForKeys.findAllIn(keysWithSeparator).toList

    val (keyJson, value) =
      keyList match {
        case keyList if keyList.isEmpty =>
          val regexForKeyJson =
            regexForKeys.findAllIn(pathField.toString()).mkString
          (Json.fromString(regexForKeyJson), oldField)
        case _ =>
          (Json.fromString(keyList.last), oldField)
      }

    val newElementForTheList =
      if (mapKeyWithList.contains(keyJson)) {
        val index = mapKeyWithList(keyJson).indexOf(value)

        Json.obj(
          ("path", Json.fromString(s"${keyList.mkString(".")}.[$index]")),
          ("old", oldField)
        )
      } else {
        val pathString = pathField.toString()
        val jsonPath = Json.fromString(
          pathString.substring(2, pathString.length - 1).replace("/", ".")
        )

        Json.obj(
          ("path", jsonPath),
          ("old", oldField)
        )
      }

    (changeList, deleteList ++ List(newElementForTheList), addList)
  }

  private def makeSolutionForAddOperation(
      pathField: Json,
      newField: Json,
      mapKeyWithList: Map[Json, List[Json]]
  )(
      changeList: List[Json],
      deleteList: List[Json],
      addList: List[Json]
  ): (List[Json], List[Json], List[Json]) = {
    val keysWithSeparator =
      regexForKeysWithSeparator.findAllIn(pathField.toString()).mkString
    val keyList = regexForKeys.findAllIn(keysWithSeparator).toList

    val (keyJson, value) =
      keyList match {
        case keyList if keyList.isEmpty =>
          val regexForKeyJson =
            regexForKeys.findAllIn(pathField.toString()).mkString
          (Json.fromString(regexForKeyJson), newField)
        case _ =>
          (Json.fromString(keyList.last), newField)
      }

    val newElementForTheList =
      if (mapKeyWithList.contains(keyJson)) {
        val index = mapKeyWithList(keyJson).indexOf(value)

        Json.obj(
          ("path", Json.fromString(s"${keyList.mkString(".")}.[$index]")),
          ("new", newField)
        )
      } else {
        val pathString = pathField.toString()
        val jsonPath = Json.fromString(
          pathString.substring(2, pathString.length - 1).replace("/", ".")
        )

        Json.obj(
          ("path", jsonPath),
          ("new", newField)
        )
      }

    (changeList, deleteList, addList ++ List(newElementForTheList))
  }

}
