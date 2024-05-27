package json4s

import io.circe.Json
import org.json4s.JsonAST.JValue
import org.json4s.native.JsonMethods._

object DiffJson4s {

  def diffJson4s(
            text1: String,
            text2: String
          ): Json = {
    val json1 = parse(text1)
    val json2 = parse(text2)

    val changedJson = buildJsonFromJValues((json1 diff json2).changed, "Changed")
    val deletedJson = buildJsonFromJValues((json1 diff json2).deleted, "Deleted")
    val addedJson = buildJsonFromJValues((json1 diff json2).added, "Added")

    changedJson.deepMerge(deletedJson.deepMerge(addedJson))
  }

  private def buildJsonFromJValues(
                                    elem: JValue,
                                    text: String
                                  ): Json =
    elem.toOption match {
      case None =>
        Json.obj((text, Json.fromString("Nothing")))
      case Some(value) =>
        Json.obj((text, Json.fromString(value.values.toString)))
    }

}
