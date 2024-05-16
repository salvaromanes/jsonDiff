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
          Json.arr(Json.obj(("some", Json.fromString("thing"))),
          Json.obj(("first", Json.obj(
            ("second", Json.fromString("zubehor"))
          )))),
          Json.arr(Json.obj(("some", Json.fromString("any"))),
          Json.obj(("first", Json.obj(
            ("second", Json.fromString("cats"))
          ))))
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
          Json.arr(
            Json.obj(("list", Json.arr())),
            Json.obj(("first", Json.obj(
                ("second", Json.fromString("zubehor"))
              )))
          ),
          Json.arr(
              Json.obj(("list", Json.fromValues(Array(Json.fromString("b"))))),
              Json.obj(("first", Json.obj(
                ("second", Json.fromString("cats"))
              )))
          )
      )))
    }

    actual shouldBe expected
  }

}
