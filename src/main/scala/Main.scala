import diffson.DiffDiffson.diffDiffson
import json4s.DiffJson4s._

object Main extends App {

  val entryJson1 =
    """{
      |  "first" : "second"
      |}""".stripMargin

  val entryJson2 =
    """{
      |  "first" : "third"
      |}""".stripMargin

  println(diffJson4s(entryJson1, entryJson2))
  println(diffDiffson(entryJson1, entryJson2))

}
