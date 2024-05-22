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
      //val itJson = compareTwoIterables(valueInsideJsonList1, valueInsideJsonList2)(Iterable())
      val itJson =
        for {
          it1 <- valueInsideJsonList1
          it2 <- valueInsideJsonList2
        } yield {
          diff2Json(it1.hcursor, it2.hcursor)
        }

      buildOutJson(itJson)(Json.Null, Json.Null)
    } else {
      val diff1 = list1.diff(list2)
      val diff2 = list2.diff(list1)

      (Json.fromValues(diff1), Json.fromValues(diff2))
    }
  }

//  @tailrec
//  private def compareTwoIterables(
//                                   iterable: (Iterable[Json], Iterable[Json])
//                                 )(
//                                   outputIterable: Iterable[(Json, Json)]
//                                 ): Iterable[(Json, Json)] = iterable match {
//    case (Nil, Nil) =>
//      outputIterable
//    case (head::tail, Nil) =>
//      compareTwoIterables(tail, Nil)(outputIterable.concat(Iterable((head, Json.Null))))
//    case (Nil, head::tail) =>
//      compareTwoIterables(Nil, tail)(outputIterable.concat(Iterable((Json.Null, head))))
//    case (head1::tail1, head2::tail2) =>
//      compareTwoIterables(tail1, tail2)(outputIterable.concat(Iterable(diff2Json(head1.hcursor, head2.hcursor))))
//  }

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
        val solution = diff2Json(head._1.hcursor, head._2.hcursor)
        buildOutJson(tail)(solution)
      } else {
        val diffs = diff2Json(head._1.hcursor, head._2.hcursor)
        val solution = (outJson._1.deepMerge(diffs._1), outJson._2.deepMerge(diffs._2))
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
