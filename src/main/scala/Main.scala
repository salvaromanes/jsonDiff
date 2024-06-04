import diffson.DiffDiffsonComplete.buildSolutionWithDiffson

object Main extends App {

  val entryJson1 =
    """{
      |  "name": "John",
      |  "surname": "Smith",
      |  "cars": [
      |    { "audi": "a1" },
      |    { "renault": "clio" },
      |    { "ford": "focus" },
      |    { "audi": "a5" }
      |  ]
      |}""".stripMargin

  val entryJson2 =
    """{
      |  "name": "Jack",
      |  "surname": "Smith",
      |  "cars": [
      |    { "audi": "a1" },
      |    { "ford": "focus" }
      |  ],
      |  "nationality" : "german"
      |}""".stripMargin

  val value = buildSolutionWithDiffson(entryJson1, entryJson2)
  println(value)

}

