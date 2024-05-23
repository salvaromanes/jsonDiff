import diffWithJson4s._

object Main extends App {

  val entryJson1 =
    """{
      |  "first" : "second"
      |}""".stripMargin

  val entryJson2 =
    """{
      |  "first" : "third"
      |}""".stripMargin

  println(diff(entryJson1, entryJson2))

}
