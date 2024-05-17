package util

import io.circe._
import util.Diff.diff2Json

import scala.annotation.tailrec

object JsonList {

  def differencesBetweenJsonList(
                                  list1: Vector[Json],
                                  list2: Vector[Json],
                                ): (Json, Json) = {
    val jsonlist1 = Json.fromValues(list1)
    val jsonlist2 = Json.fromValues(list2)

    val valueInsideJsonList1 = jsonlist1.hcursor.values.get
    val valueInsideJsonList2 = jsonlist2.hcursor.values.get

    if(isNested(valueInsideJsonList1) && isNested(valueInsideJsonList2)) {
      val itJson =
        for {
          it1 <- valueInsideJsonList1
          it2 <- valueInsideJsonList2
        } yield {
          diff2Json(it1.hcursor, it2.hcursor)
        }

      val emptyJson = (Json.Null, Json.Null)
      buildOutJson(itJson)(emptyJson)
    } else {
      val diff1 = list1.diff(list2)
      val diff2 = list2.diff(list1)

      (Json.fromValues(diff1), Json.fromValues(diff2))
    }
  }

  @tailrec
  private def buildOutJson(
                            iterable: Iterable[(Json, Json)]
                          )(
                            outJson: (Json, Json)
                          ): (Json, Json) = iterable.toList match {
    case Nil =>
      outJson
    case head::tail =>
      if(outJson._1.isNull && outJson._2.isNull) {
        val solution = (head._1, head._2)
        buildOutJson(tail)(solution)
      } else {
        val solution = (outJson._1.deepMerge(head._1), outJson._2.deepMerge(head._2))
        buildOutJson(tail)(solution)
      }
  }

  private def isNested(
                        iterable: Iterable[Json]
                      ): Boolean = {
    val noNested = iterable.forall(elem => elem.isNumber || elem.isString)
    !noNested
  }

}
