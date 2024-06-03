package jsonDiff

import diffson.DiffDiffson.buildSolutionWithDiffson
import io.circe.Json

import scala.annotation.tailrec

object jsonDiffListOfKeys {

  def buildJsonDiffSolution(
                             text1: String,
                             text2: String
                           ): Json = {
    val maybeDiff = buildSolutionWithDiffson(text1, text2)

    maybeDiff match {
      case Right(differences) =>
        diffTwoJson(differences)
      case _ =>
        throw new RuntimeException("Error in buildJsonDiffSolution: Left has been returning")
    }
  }

  private def diffTwoJson(
                           differences: Json
                         ): Json = {
    val differencesOldToNewCursor = differences.hcursor

    val maybeSolution =
      for {
        differentKeys <- differencesOldToNewCursor.keys.map(k => k.toList)
      } yield {
        buildListOfDifferentKeys(differentKeys)(List())
      }

    maybeSolution match {
      case Some(value) =>
        Json.fromValues(value)
    }
  }

  @tailrec
  private def buildListOfDifferentKeys(
                                        listOfStringKeys: List[String]
                                      )(
                                        listOfJson: List[Json]
                                      ): List[Json] = listOfStringKeys match {
    case Nil =>
      listOfJson
    case head::tail if listOfJson.isEmpty =>
      buildListOfDifferentKeys(tail)(List(Json.fromString(head)))
    case head::tail =>
      buildListOfDifferentKeys(tail)(listOfJson.concat(List(Json.fromString(head))))
  }

}
