import jsonDiff.jsonDiff.printDiffTwoJsonWithColor

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
      |  ],
      |  "nationality" : "german"
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

  printDiffTwoJsonWithColor(entryJson1, entryJson2)

}
