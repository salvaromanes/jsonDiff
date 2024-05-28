import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import json4s.DiffJson4s._

class DiffTestJson4s extends AnyFlatSpec with Matchers {

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

    val actual = diffJson4s(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Nothing")),
        ("Deleted", Json.fromString("Nothing")),
        ("Changed", Json.fromString("Nothing"))
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

    val actual = diffJson4s(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Nothing")),
        ("Deleted", Json.fromString("Nothing")),
        ("Changed", Json.fromString("Nothing"))
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

    val actual = diffJson4s(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Nothing")),
        ("Deleted", Json.fromString("Nothing")),
        ("Changed", Json.fromString("Nothing"))
      )
    }

    actual shouldBe expected
  }

  // Differences inside json bodies

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

    val actual = diffJson4s(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Nothing")),
        ("Deleted", Json.fromString("Nothing")),
        ("Changed", Json.fromString("Map(first -> third)"))
      )
    }

    actual shouldBe expected
  }

  "Two json with a different list" should
    "return a no differences Json" in {
    val entryJson1 =
      """{
        |  "first" : ["second"]
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : ["third"]
        |}""".stripMargin

    val actual = diffJson4s(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Nothing")),
        ("Deleted", Json.fromString("Nothing")),
        ("Changed", Json.fromString("Map(first -> third)"))
      )
    }

    actual shouldBe expected
  }

  "Two json with a different nested object" should
    "return a no differences Json" in {
    val entryJson1 =
      """{
        |  "first" : {
        |    "second" : "hello"
        |  }
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "first" : {
        |    "second" : "bye"
        |  }
        |}""".stripMargin

    val actual = diffJson4s(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Nothing")),
        ("Deleted", Json.fromString("Nothing")),
        ("Changed", Json.fromString("Map(first -> Map(second -> bye))"))
      )
    }

    actual shouldBe expected
  }

  "Two json with changes, deleted and added elements" should
    "return a no differences Json" in {
    val entryJson1 =
      """{
        |  "first" : "hello",
        |  "second" : {
        |    "second" : "hello"
        |  }
        |}""".stripMargin

    val entryJson2 =
      """{
        |  "second" : {
        |    "second" : "bye"
        |  },
        |  "third" : "bye"
        |}""".stripMargin

    val actual = diffJson4s(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("Added", Json.fromString("Map(third -> bye)")),
        ("Deleted", Json.fromString("Map(first -> hello)")),
        ("Changed", Json.fromString("Map(second -> Map(second -> bye))"))
      )
    }

    actual shouldBe expected
  }

}
