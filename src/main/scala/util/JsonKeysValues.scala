package util

import io.circe.Json

case class JsonKeysValues(key: String, value: Json) {

  def toJson: Json =
    Json.obj(
      (key, value)
    )

}
