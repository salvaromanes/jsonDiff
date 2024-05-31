import jsonDiff._

object Main extends App {

  val entryJson1 =
    """{
      |  "name": "John",
      |  "surname": "Smith",
      |  "address": {
      |    "home" : "C/Here",
      |    "number" : "123",
      |    "location" : {
      |      "town" : "Malaga",
      |      "city" : "Malaga",
      |      "country" : "Spain"
      |    }
      |  }
      |}""".stripMargin

  val entryJson2 =
    """{
      |  "name": "Jack",
      |  "surname": "Smith",
      |  "address": {
      |    "home" : "C/Here",
      |    "number" : "123",
      |    "location" : {
      |      "town" : "Malaga",
      |      "city" : "Malaga",
      |      "country" : "Colombia"
      |    }
      |  }
      |}""".stripMargin

//  println(jsonDiff.printDiffTwoJsonWithColor(entryJson1, entryJson2))
//  println(jsonDiff.buildJsonDiffSolution(entryJson1, entryJson2))
  println(jsonDiffListOfKeys.buildJsonDiffSolution(entryJson1, entryJson2))

}
