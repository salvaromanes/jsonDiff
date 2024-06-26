import diffson.DiffDiffson.buildSolutionWithDiffson

object Main extends App {

  val entryJson1 =
    """{
      |  "name": "John",
      |  "surname": "Smith",
      |  "transport" : [{
      |    "street" : [{
      |      "cars": [
      |        { "audi": "a1" },
      |        { "renault": "clio" },
      |        { "ford": "focus" },
      |        { "audi": "a5" }
      |      ]
      |    }]
      |  }]
      |}""".stripMargin

  val entryJson2 =
    """{
      |  "name": "Jack",
      |  "surname": "Smith",
      |  "transport" : [{
      |    "street" : [{
      |      "cars": [
      |        { "audi": "a1" },
      |        { "ford": "focus" }
      |      ]
      |    }]
      |  }],
      |  "nationality" : "german"
      |}""".stripMargin

  val value = buildSolutionWithDiffson(entryJson1, entryJson2)
  println(value)

}
