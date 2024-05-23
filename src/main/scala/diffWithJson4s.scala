import io.circe.Json
import org.json4s.native.JsonMethods._

object diffWithJson4s {

  def diff(
            text1: String,
            text2: String
          ): Json = {
    val json1 = parse(text1)
    val json2 = parse(text2)

    val changes = (json1 diff json2).changed
    val delete = (json1 diff json2).deleted
    val add = (json1 diff json2).added

    val changedJson =
      changes.toOption match {
        case None =>
          Json.obj(("Changes", Json.fromString("Nothing")))
        case Some(value) =>
          Json.obj(("Changes", Json.fromString(value.values.toString)))
      }

    val deletedJson =
      delete.toOption match {
        case None =>
          Json.obj(("Deleted", Json.fromString("Nothing")))
        case Some(value) =>
          Json.obj(("Deleted", Json.fromString(value.values.toString)))
      }

    val addedJson =
      add.toOption match {
        case None =>
          Json.obj(("Added", Json.fromString("Nothing")))
        case Some(value) =>
          Json.obj(("Added", Json.fromString(value.values.toString)))
      }

    changedJson.deepMerge(deletedJson.deepMerge(addedJson))
  }

}
