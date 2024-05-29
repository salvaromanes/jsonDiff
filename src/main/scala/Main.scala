import jsonDiff.jsonDiff.buildJsonDiffSolution

object Main extends App {

  val entryJson1 =
    """{
      |  "name": "John",
      |  "surname": "Smith",
      |  "cars": [
      |    { "audi": "a1" },
      |    { "renault": "clio" },
      |    { "audi": "a5" },
      |    { "ford": "focus" }
      |  ]
      |}""".stripMargin

  val entryJson2 =
    """{
      |  "name": "Jack",
      |  "surname": "Smith",
      |  "cars": [
      |    { "audi": "a1" },
      |    { "ford": "focus" }
      |  ]
      |}""".stripMargin

  println(buildJsonDiffSolution(entryJson1, entryJson2))

}
