package diffson

import diffson.FormatBuilder.buildSolutionWithCorrectFormat
import diffson.circe._
import diffson.jsonpatch._
import diffson.jsonpatch.lcsdiff.remembering._
import diffson.lcs._
import io.circe._
import io.circe.parser._

object DiffDiffson {

  def buildSolutionWithDiffson(
      text1: String,
      text2: String
  ): Json = {
    val maybeJson1 = parse(text1)
    val maybeJson2 = parse(text2)

    (maybeJson1, maybeJson2) match {
      case (Right(v1), Right(v2)) =>
        val maybeArrayOfDifferences = diffDiffson(v1, v2).asArray

        val listOfDifferences =
          maybeArrayOfDifferences match {
            case Some(arrayOfDifferences) =>
              arrayOfDifferences.toList
            case None =>
              List()
          }

        val cursor = v1.hcursor
        val keys = cursor.keys.get.toList
        val mapKey = mapKeyWithList(cursor, keys)(Map())

        buildSolutionWithCorrectFormat(listOfDifferences, mapKey)(
          List(),
          List(),
          List()
        )
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
      cursor.downField(head).as[Json] match {
        case Right(value) if value.isArray =>
          val newCursor = value.asArray.get.toList.head.hcursor
          val newKeys = newCursor.keys.getOrElse(List()).toList

          val newOutMap =
            if (newKeys.nonEmpty) {
              mapKeyForIndexedLists(
                newCursor,
                newKeys
              )(
                Map(
                  outMap.head._1.deepMerge(Json.fromString(s".$head")) -> List()
                )
              )
            } else {
              Map(
                outMap.head._1.deepMerge(
                  Json.fromString(s".$head")
                ) -> value.asArray.get.toList
              )
            }

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

  private def mapKeyForIndexedLists(
      cursor: HCursor,
      listOfKeys: List[String]
  )(
      outMap: Map[Json, List[Json]]
  ): Map[Json, List[Json]] = listOfKeys match {
    case Nil =>
      outMap
    case head :: _ =>
      val maybeValue = cursor.downField(head).as[Json]

      val newOutMap =
        for {
          value <- maybeValue
        } yield {
          val newCursor = value.hcursor
          val keys = newCursor.keys.getOrElse(List()).toList

          mapKeyForIndexedLists(
            newCursor,
            keys
          )(
            Map(outMap.head._1.deepMerge(Json.fromString(head)) -> List())
          )
        }

      newOutMap match {
        case Right(map) =>
          map
        case Left(_) =>
          Map.empty
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
