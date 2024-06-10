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

//  val entryJson1 =
//    """{
//      |  "name": "John",
//      |  "surname": "Smith",
//      |  "cars": [
//      |    { "audi": "a1" },
//      |    { "renault": "clio" },
//      |    { "ford": "focus" },
//      |    { "audi": "a5" }
//      |  ]
//      |}""".stripMargin
//
//  val entryJson2 =
//    """{
//      |  "name": "Jack",
//      |  "surname": "Smith",
//      |  "cars": [
//      |    { "audi": "a1" },
//      |    { "ford": "focus" }
//      |  ],
//      |  "nationality" : "german"
//      |}""".stripMargin

//  val entryJson1 =
//    """{
//      |  "name": "John",
//      |  "surname": "Smith",
//      |  "address": {
//      |    "country" : "Spain",
//      |    "town/city" : "Malaga",
//      |    "street" : "Here",
//      |    "postal code" : "1234",
//      |    "house number" : "1"
//      |  },
//      |  "bank data" : {
//      |    "IBAN" : "ES123456789",
//      |    "SWIFT" : "AAAA-BB-CC-123",
//      |    "entity" : "bank 1",
//      |    "location" : {
//      |      "country" : "Spain",
//      |      "town/city" : "Malaga",
//      |      "street" : "There",
//      |      "postal code" : "1234",
//      |      "house number" : "1"
//      |    }
//      |  },
//      |  "married" : "false"
//      |}""".stripMargin
//
//  val entryJson2 =
//    """{
//      |  "name": "Jack",
//      |  "surname": "Smith",
//      |  "address": {
//      |    "country" : "Spain",
//      |    "town/city" : "Malaga",
//      |    "street" : "Here",
//      |    "postal code" : "1234",
//      |    "house number" : "2"
//      |  },
//      |  "bank data" : {
//      |    "IBAN" : "ES123456789",
//      |    "SWIFT" : "AAAA-BB-CC-123",
//      |    "entity" : "bank 1",
//      |    "location" : {
//      |      "country" : "Spain",
//      |      "town/city" : "Malaga",
//      |      "street" : "There",
//      |      "postal code" : "1234",
//      |      "house number" : "2"
//      |    }
//      |  },
//      |  "working status" : "active"
//      |}""".stripMargin

  val value = buildSolutionWithDiffson(entryJson1, entryJson2)
  println(value)

}
