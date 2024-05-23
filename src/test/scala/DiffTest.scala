import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import diffWithJson4s._

class DiffTest extends AnyFlatSpec with Matchers {

  // Same things inside json bodies

  "Two same json with a field" should
    "return a no differences Json" in {
    val entryJson1 =
      """{
        |  "first" : "second"
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : "second"
        |}""".stripMargin

    val actual = diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Nothing")),
        ("Deleted", Json.fromString("Nothing")),
        ("Changes", Json.fromString("Nothing"))
      )
    }

    actual shouldBe expected
  }

  "Two same json with a list" should
    "return a no differences Json" in {
    val entryJson1 =
      """{
        |  "first" : [
        |    "second"
        |  ]
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : [
        |    "second"
        |  ]
        |}""".stripMargin

    val actual = diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Nothing")),
        ("Deleted", Json.fromString("Nothing")),
        ("Changes", Json.fromString("Nothing"))
      )
    }

    actual shouldBe expected
  }

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

    val actual = diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Nothing")),
        ("Deleted", Json.fromString("Nothing")),
        ("Changes", Json.fromString("Nothing"))
      )
    }

    actual shouldBe expected
  }

  // Different things inside json bodies

  "Two json with a different field" should
    "return a no differences Json" in {
    val entryJson1 =
      """{
        |  "first" : "second"
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : "third"
        |}""".stripMargin

    val actual = diff(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Nothing")),
        ("Deleted", Json.fromString("Nothing")),
        ("Changes", Json.fromString("Map(first -> third)"))
      )
    }

    actual shouldBe expected
  }

}
