import org.json4s.{JObject, JValue}
import org.json4s.native.JsonMethods._

object Main extends App {

  val entryJson1 =
    parse("""{
      |  "first" : [{
      |    "second" : "zubehor"
      |  },{
      |    "third" : "zubehor"
      |  }]
      |}""".stripMargin)

  val entryJson2 =
    parse("""{
      |  "first" : [{
      |    "second" : "cats"
      |  },{
      |    "four" : "hello"
      |  },{
      |    "five" : "bye"
      |  }]
      |}""".stripMargin)

  val changes = (entryJson1 diff entryJson2).changed
  val delete = (entryJson1 diff entryJson2).deleted
  val add = (entryJson1 diff entryJson2).added

  val merge = changes.merge(delete.merge(add))


  println(JObject(List(changes, delete, add)))

}
