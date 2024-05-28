import diffson.DiffDiffson.diffDiffson
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DiffTestDiffson extends AnyFlatSpec with Matchers{

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

    val actual = diffDiffson(entryJson1, entryJson2)

    val expected = {
      Json.arr()
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

    val actual = diffDiffson(entryJson1, entryJson2)

    val expected = {
      Json.arr()
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

    val actual = diffDiffson(entryJson1, entryJson2)

    val expected = {
      Json.arr()
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

    val actual = diffDiffson(entryJson1, entryJson2)

    val expected = {
      Json.arr(
        Json.obj(
          ("op", Json.fromString("replace")),
          ("path", Json.fromString("/first")),
          ("value", Json.fromString("third"))
        )
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

    val actual = diffDiffson(entryJson1, entryJson2)

    val expected = {
      Json.arr(
        Json.obj(
          ("op", Json.fromString("replace")),
          ("path", Json.fromString("/first/0")),
          ("value", Json.fromString("third"))
        )
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

    val actual = diffDiffson(entryJson1, entryJson2)

    val expected = {
      Json.arr(
        Json.obj(
          ("op", Json.fromString("replace")),
          ("path", Json.fromString("/first/second")),
          ("value", Json.fromString("bye"))
        )
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

    val actual = diffDiffson(entryJson1, entryJson2)

    val expected = {
      Json.arr(
        Json.obj(
          ("op", Json.fromString("remove")),
          ("path", Json.fromString("/first"))
        ),
        Json.obj(
          ("op", Json.fromString("replace")),
          ("path", Json.fromString("/second/second")),
          ("value", Json.fromString("bye"))
        ),
        Json.obj(
          ("op", Json.fromString("add")),
          ("path", Json.fromString("/third")),
          ("value", Json.fromString("bye"))
        )
      )
    }

    actual shouldBe expected
  }

}
