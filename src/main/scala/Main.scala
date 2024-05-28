import diffson.DiffDiffson.buildSolutionWithDiffson

object Main extends App {

  val entryJson1 =
    """{
      |  "zero": "one",
      |  "first": [
      |    { "second": "a" },
      |    { "second": "b" },
      |    { "second": "c" },
      |    { "second": "d" },
      |    { "second": "e" }
      |  ]
      |}""".stripMargin

  val entryJson2 =
    """{
      |  "zero": "one",
      |  "first": [
      |    { "second": "a" },
      |    { "second": "c" },
      |    { "second": "e" }
      |  ]
      |}""".stripMargin

  println(buildSolutionWithDiffson(entryJson1, entryJson2))

}
