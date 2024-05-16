import util.Diff.diff

object Main extends App {

  private val entryJson1 =
    """{
      |  "list" : [{
      |    "first" : "zubehor",
      |    "second" : "zubehor"
      |  }]
      |}""".stripMargin

  private val entryJson2 =
    """{
      |  "list" : [{
      |    "first" : "zubehor",
      |    "second" : "cats"
      |  }]
      |}""".stripMargin

  println(diff(entryJson1, entryJson2))

}
