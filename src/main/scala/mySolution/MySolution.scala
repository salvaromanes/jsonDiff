package mySolution

import io.circe._
import io.circe.parser._

object MySolution {

  def buildSolution(
                      text1: String,
                      text2: String
                   ): Json = {
    val maybeJson1 = parse(text1)
    val maybeJson2 = parse(text2)

    (maybeJson1, maybeJson2) match {
      case (Right(v1), Right(v2)) =>
        val diffs = diffTwoJson(v1, v2)
        Json.obj(("differences", diffs))
      case _ =>
        Json.fromString("something was wrong")
    }
  }

  private def diffTwoJson(
                           j1: Json,
                           j2: Json
                         ): Json = {
    val c1 = j1.hcursor
    val c2 = j2.hcursor

    lookForSameElements(c1, c2)
  }

  private def lookForSameElements(c1: HCursor, c2: HCursor): Json = {
    //Buscar los elementos que son iguales para unirlos con los que son
    //diferentes despues
    Json.Null
  }

}
