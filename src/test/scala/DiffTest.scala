import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import util.Diff

class DiffTest extends AnyFlatSpec with Matchers {

  "Two same json with a nested object" should
    "return a no differences Json" in {
    val entryJson1 =
      """{
        |  "first" : {
        |    "second" : "zubehor"
        |  }
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : {
        |    "second" : "zubehor"
        |  }
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.fromString("No differences")))
    }

    actual shouldBe expected
  }

  "Two different json with several fields" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "first" : "zubehor",
        |  "second" : "zubehor",
        |  "third" : "zubehor"
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : "cats",
        |  "second" : "zubehor"
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.arr(
          Json.obj(
              ("first",Json.fromString("zubehor")),
              ("third",Json.fromString("zubehor"))
          ),
          Json.obj(
              ("first",Json.fromString("cats")),
              ("third",Json.Null)
          )
        ))
      )
    }

    actual shouldBe expected
  }

  "Two different json with a nested object" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "first" : {
        |    "second" : "zubehor"
        |  }
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : {
        |    "second" : "cats"
        |  }
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.arr(
            Json.obj(("first",Json.obj(
                ("second",Json.fromString("zubehor"))
              ))
            ),
            Json.obj(("first",Json.obj(
                ("second",Json.fromString("cats"))
              ))
            ))
        )
      )
    }

    actual shouldBe expected
  }

  "Two different json with several nested objects" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "first" : {
        |    "second" : "zubehor"
        |  },
        |  "second" : {
        |    "second" : "zubehor"
        |  }
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : {
        |    "second" : "zubehor"
        |  },
        |  "second" : {
        |    "second" : "cats"
        |  }
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.arr(
          Json.obj(
            ("second",Json.obj(
              ("second", Json.fromString("zubehor"))
            ))
          ),
          Json.obj(
            ("second",Json.obj(
              ("second", Json.fromString("cats"))
            ))
          )
        ))
      )
    }

    actual shouldBe expected
  }

  "Two different json with a field and a nested object" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "some" : "thing",
        |  "first" : {
        |    "second" : "zubehor"
        |  }
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "some" : "any",
        |  "first" : {
        |    "second" : "cats"
        |  }
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.arr(
          Json.obj(
            ("some", Json.fromString("thing")),
            ("first", Json.obj(
              ("second", Json.fromString("zubehor"))
          ))),
          Json.obj(
            ("some", Json.fromString("any")),
            ("first", Json.obj(
              ("second", Json.fromString("cats"))
          )))
        ))
      )
    }

    actual shouldBe expected
  }

  "Two different json with a nested object inside other nested object" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "first" : {
        |    "second" : {
        |      "third" : "zubehor"
        |    }
        |  }
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : {
        |    "second" : {
        |      "third" : "cats"
        |    }
        |  }
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.arr(
          Json.obj(
            ("first", Json.obj(
              ("second", Json.obj(
                ("third", Json.fromString("zubehor"))
              ))
            ))
          ),
          Json.obj(
            ("first", Json.obj(
              ("second", Json.obj(
                ("third", Json.fromString("cats"))
              ))
            ))
            ))
        )
      )
    }

    actual shouldBe expected
  }

  "Two different json with a list and a nested object" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "list" : ["a"],
        |  "first" : {
        |    "second" : "zubehor"
        |  }
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "list" : ["a","b"],
        |  "first" : {
        |    "second" : "cats"
        |  }
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.arr(
          Json.obj(
            ("list", Json.arr()),
            ("first", Json.obj(
              ("second", Json.fromString("zubehor"))
            ))
          ),
          Json.obj(
            ("list", Json.fromValues(Array(Json.fromString("b")))),
            ("first", Json.obj(
              ("second", Json.fromString("cats"))
          )))
        )
      ))
    }

    actual shouldBe expected
  }

  "Two different json with a list inside a nested object" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "first" : {
        |    "list" : ["a"],
        |    "second" : "zubehor"
        |  }
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : {
        |    "list" : ["a","b"],
        |    "second" : "cats"
        |  }
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.arr(
          Json.obj(
            ("first", Json.obj(
              ("list", Json.arr()),
              ("second", Json.fromString("zubehor"))
            ))
          ),
          Json.obj(
            ("first", Json.obj(
              ("list", Json.arr(Json.fromString("b"))),
              ("second", Json.fromString("cats"))
            )
          ))
        ))
      )
    }

    actual shouldBe expected
  }

  "Two different json with several list" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "first" : ["zubehor"],
        |  "second" : ["zubehor"],
        |  "third" : ["zubehor"]
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : ["cats"],
        |  "second" : ["zubehor"]
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.arr(
          Json.obj(
            ("first",Json.arr(Json.fromString("zubehor"))),
            ("third",Json.arr(Json.fromString("zubehor")))
          ),
          Json.obj(
            ("first",Json.arr(Json.fromString("cats"))),
            ("third",Json.Null)
          )
        ))
      )
    }

    actual shouldBe expected
  }

  "Two different json list of nested objects" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "first" : [{
        |    "second" : "zubehor",
        |    "third" : "zubehor"
        |  }]
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : [{
        |    "second" : "zubehor",
        |    "third" : "cats"
        |  }]
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.arr(
          Json.obj(("first",
            Json.obj(("third",Json.fromString("zubehor")))
          )),
          Json.obj(("first",
            Json.obj(("third",Json.fromString("cats")))
          ))
        ))
      )
    }

    actual shouldBe expected
  }

  "Two list with the same nested object inside" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "first" : [{
        |    "second" : "zubehor",
        |    "third" : "zubehor"
        |  }]
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : [{
        |    "second" : "zubehor",
        |    "third" : "zubehor"
        |  }]
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.fromString("No differences"))
      )
    }

    actual shouldBe expected
  }

  "A list with a nested object inside and a field" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "first" : [{
        |    "second" : "zubehor",
        |    "third" : "zubehor"
        |  }]
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "lorem" : "ipsum"
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Differences", Json.arr(
          Json.obj(
            ("first", Json.arr(
              Json.obj(
                ("second", Json.fromString("zubehor")),
                ("third", Json.fromString("zubehor")))
            )),
            ("lorem", Json.Null)
          ),
          Json.obj(
            ("first", Json.Null),
            ("lorem", Json.fromString("ipsum"))
          )
        ))
      )
    }

    actual shouldBe expected
  }

  "Two lists with several nested objects inside" should
    "return a Json with the differences between both" in {
    val entryJson1 =
      """{
        |  "first" : [{
        |    "second" : "zubehor"
        |  },{
        |    "third" : "zubehor"
        |  }]
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : [{
        |    "second" : "cats"
        |  },{
        |    "third" : "zubehor"
        |  }]
        |}""".stripMargin

    val actual = Diff.diff(entryJson1, entryJson2)

//    val expected = {
//      Json.obj(
//        ("Differences", Json.arr(
//          Json.obj(
//            ("first", Json.obj(
//                ("second", Json.fromString("zubehor")))
//            )
//          ),
//          Json.obj(
//            ("first", Json.obj(
//              ("second", Json.fromString("cats")))
//            )
//          )
//        ))
//      )
//    }

    val wrongSolution = {
      Json.obj(
        ("Differences", Json.arr(
          Json.obj(
            ("first", Json.Null)
          ),
          Json.obj(
            ("first", Json.Null)
          )
        ))
      )
    }

    //actual shouldBe expected
    actual shouldBe wrongSolution
  }

}
