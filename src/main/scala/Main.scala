import diffson.DiffDiffsonComplete.buildSolutionWithDiffson

object Main extends App {

  val entryJson1 =
    """{
      |  "name": "John",
      |  "surname": "Smith",
      |  "address": {
      |    "country" : "Spain",
      |    "town/city" : "Malaga",
      |    "street" : "Here",
      |    "postal code" : "1234",
      |    "house number" : "1"
      |  }
      |}""".stripMargin

  val entryJson2 =
    """{
      |  "name": "Jack",
      |  "surname": "Nick",
      |  "bank data" : {
      |    "IBAN" : "ES987654321",
      |    "SWIFT" : "AAAA-BB-CC-321",
      |    "entity" : "bank 2",
      |    "location" : {
      |      "country" : "Spain",
      |      "town/city" : "Malaga",
      |      "street" : "Here",
      |      "postal code" : "4321",
      |      "house number" : "25"
      |    }
      |  }
      |}""".stripMargin

  val value = buildSolutionWithDiffson(entryJson1, entryJson2)
  println(value)

}

