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

        buildSolutionWithCorrectFormat(listOfDifferences)(List(), List(), List())
      case _ =>
        Json.fromString("Something went wrong")
    }
  }

  @tailrec
  private def buildSolutionWithCorrectFormat(
                                              differences: List[Json]
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
      val maybeOperation = head.hcursor.downField("op").as[Json]

      maybeOperation match {
        case Right(operation) =>
          val maybePathField = head.hcursor.downField("path").as[Json]
          val maybeNewField = head.hcursor.downField("value").as[Json]
          val maybeOldField = head.hcursor.downField("old").as[Json]

          val listOfJsonDifferences =
            operation match {
              case op if op.equals(Json.fromString("replace")) =>
                val newElementOfList =
                  for {
                    pathField <- maybePathField
                    newField <- maybeNewField
                    oldField <- maybeOldField
                  } yield {
                    Json.obj(
                      ("path", pathField),
                      ("new", newField),
                      ("old", oldField)
                    )
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
                    Json.obj(
                      ("path", pathField),
                      ("old", oldField)
                    )
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
                    Json.obj(
                      ("path", pathField),
                      ("new", newField)
                    )
                  }

                newElementOfList match {
                  case Right(value) =>
                    (changeList, deleteList, addList ++ List(value))
                }
            }

          buildSolutionWithCorrectFormat(tail)(listOfJsonDifferences._1, listOfJsonDifferences._2, listOfJsonDifferences._3)
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
