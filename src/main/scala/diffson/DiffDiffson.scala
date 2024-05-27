package diffson

import diffson.lcs._
import diffson.circe._
import diffson.jsonpatch._
import diffson.jsonpatch.lcsdiff._

import io.circe._
import io.circe.parser._

object DiffDiffson {

  def diffDiffson(
                   text1: String,
                   text2: String
                 ): Json = {
    implicit val lcs: Patience[Json] = new Patience[Json]
    val encoder = Encoder[JsonPatch[Json]]

    val json1 = parse(text1)
    val json2 = parse(text2)

    val patch =
      for {
        json1 <- json1
        json2 <- json2
      } yield diff(json1, json2)

    patch match {
      case Left(error) =>
        Json.fromString(error.getMessage())
      case Right(value) =>
        encoder(value)
    }
  }

}
