import diffson.DiffDiffson.diffDiffson
import json4s.DiffJson4s._

object Main extends App {

  val entryJson1 =
    """{
      |  "first" : "hello",
      |  "second" : {
      |    "second" : "hello"
      |  }
      |}""".stripMargin

  val entryJson2 =
    """{
      |  "second" : {
      |    "second" : "bye"
      |  },
      |  "third" : "bye"
      |}""".stripMargin

  // Differences using Json4s library -> efficient solution

  println("Json4s solution:")
  println(diffJson4s(entryJson1, entryJson2))

  // Differences using Diffson library -> looks pretty and easy to read

  println("Diffson solution:")
  println(diffDiffson(entryJson1, entryJson2))

}
