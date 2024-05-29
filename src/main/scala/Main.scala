import jsonDiff.jsonDiff.{buildJsonDiffSolution, printDiffTwoJsonWithColor}

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

  // This one show the solution using _old, _new and _eq names for the fields

//  println(buildJsonDiffSolution(entryJson1, entryJson2))

  // This one show the solution using the color pattern (Green = new, Red = old, White = with no changes)

  printDiffTwoJsonWithColor(entryJson1, entryJson2)

}
