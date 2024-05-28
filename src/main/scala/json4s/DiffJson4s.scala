package json4s

import org.json4s.JsonAST.JValue
import org.json4s.native.JsonMethods._
import org.json4s.{JNull, JObject}

object DiffJson4s {

  def diffJson4s(
      text1: String,
      text2: String
  ): JValue = {
    val json1 = parse(text1)
    val json2 = parse(text2)

    val diffJson = json1 diff json2

    val addedJson = JObject("added" -> diffJson.added.toOption.getOrElse(JNull))
    val changedJson = JObject("changed" -> diffJson.changed.toOption.getOrElse(JNull))
    val deletedJson = JObject("deleted" -> diffJson.deleted.toOption.getOrElse(JNull))

    addedJson.merge(changedJson).merge(deletedJson)
  }

}
