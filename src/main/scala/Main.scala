import util.Diff.diff

object Main extends App {

  val entryJson1 =
    """{
      |  "first" : [{
      |    "second" : "zubehor",
      |    "third" : "zubehor"
      |  }]
      |}""".stripMargin

  val entryJson2 =
    """{
      |  "first" : [{
      |    "second" : "zubehor",
      |    "third" : "cats"
      |  }]
      |}""".stripMargin

  println(diff(entryJson1, entryJson2))

}
