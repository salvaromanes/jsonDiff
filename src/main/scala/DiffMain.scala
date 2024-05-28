import diffson._
import diffson.lcs._
import diffson.circe._
import diffson.jsonpatch._
import diffson.jsonpatch.lcsdiff.remembering._

import io.circe._
import io.circe.parser._

object DiffMain extends App {

  implicit val lcs: Patience[Json] = new Patience[Json]
  val encoder = Encoder[JsonPatch[Json]]

  val entryJson1 = parse(
    """{
      |  "zero": "one",
      |  "first": [ { "second": "a" }, { "second": "b" },{ "second": "c"},{ "second": "d"},{ "second": "e"}]
      |}""".stripMargin
  )

  val entryJson2 = parse(
    """{
      |  "first" : [{ "second": "a"},{ "second": "c"},{ "second": "b"}],
      |  "zero": "one"
      |}""".stripMargin
  )

  private val patch =
    for {
      json1 <- entryJson1
      json2 <- entryJson2
    } yield diff(json1, json2)

  patch.map(encoder.apply).foreach(println)

}
