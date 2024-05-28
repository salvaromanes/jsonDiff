package diffson

import diffson.circe._
import diffson.jsonmergepatch._

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
        val diffs = diffDiffson(v1, v2)
        Json.obj(("differences", diffs))
      case _ =>
        Json.fromString("something was wrong")
    }
  }

  private def diffDiffson(
                   json1: Json,
                   json2: Json
                 ): Json = {
    val encoder = Encoder[JsonMergePatch[Json]]
    encoder(diff(json1, json2))
  }

}
