import diffson.DiffDiffson.buildSolutionWithDiffson
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DiffTestJsonDiff extends AnyFlatSpec with Matchers {

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

      val actual = buildSolutionWithDiffson(entryJson1, entryJson2)

      val expected = {
        Json.obj(
          ("change", Json.arr()),
          ("remove", Json.arr()),
          ("add", Json.arr())
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

      val actual = buildSolutionWithDiffson(entryJson1, entryJson2)

      val expected = {
        Json.obj(
          ("change", Json.arr()),
          ("remove", Json.arr()),
          ("add", Json.arr())
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

      val actual = buildSolutionWithDiffson(entryJson1, entryJson2)

      val expected = {
        Json.obj(
          ("change", Json.arr()),
          ("remove", Json.arr()),
          ("add", Json.arr())
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

      val actual = buildSolutionWithDiffson(entryJson1, entryJson2)

      val expected = {
        Json.obj(
          (
            "change",
            Json.arr(
              Json.obj(
                ("path", Json.fromString("first")),
                ("new", Json.fromString("third")),
                ("old", Json.fromString("second"))
              )
            )
          ),
          ("remove", Json.arr()),
          ("add", Json.arr())
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

      val actual = buildSolutionWithDiffson(entryJson1, entryJson2)

      val expected = {
        Json.obj(
          (
            "change",
            Json.arr(
              Json.obj(
                ("path", Json.fromString("first[0]")),
                ("new", Json.fromString("third")),
                ("old", Json.fromString("second"))
              )
            )
          ),
          ("remove", Json.arr()),
          ("add", Json.arr())
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

      val actual = buildSolutionWithDiffson(entryJson1, entryJson2)

      val expected = {
        Json.obj(
          (
            "change",
            Json.arr(
              Json.obj(
                ("path", Json.fromString("first.second")),
                ("new", Json.fromString("bye")),
                ("old", Json.fromString("hello"))
              )
            )
          ),
          ("remove", Json.arr()),
          ("add", Json.arr())
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

      val actual = buildSolutionWithDiffson(entryJson1, entryJson2)

      val expected = {
        Json.obj(
          (
            "change",
            Json.arr(
              Json.obj(
                ("path", Json.fromString("second.second")),
                ("new", Json.fromString("bye")),
                ("old", Json.fromString("hello"))
              )
            )
          ),
          (
            "remove",
            Json.arr(
              Json.obj(
                ("path", Json.fromString("first")),
                ("old", Json.fromString("hello"))
              )
            )
          ),
          (
            "add",
            Json.arr(
              Json.obj(
                ("path", Json.fromString("third")),
                ("new", Json.fromString("bye"))
              )
            )
          )
        )
      }

      actual shouldBe expected
    }

  "Two complete json with lists" should
    "return a no differences Json" in {
      val entryJson1 =
        """{
        |  "name": "John",
        |  "surname": "Smith",
        |  "cars": [
        |    { "audi": "a1" },
        |    { "renault": "clio" },
        |    { "audi": "a5" },
        |    { "ford": "focus" }
        |  ],
        |  "nationality" : "German"
        |}""".stripMargin

      val entryJson2 =
        """{
        |  "name": "Jack",
        |  "surname": "Smith",
        |  "cars": [
        |    { "audi": "a1" },
        |    { "ford": "focus" }
        |  ]
        |}""".stripMargin

      val actual = buildSolutionWithDiffson(entryJson1, entryJson2)

      val expected = {
        Json.obj(
          (
            "change",
            Json.arr(
              Json.obj(
                ("path", Json.fromString("name")),
                ("new", Json.fromString("Jack")),
                ("old", Json.fromString("John"))
              )
            )
          ),
          (
            "remove",
            Json.arr(
              Json.obj(
                ("path", Json.fromString("cars[2]")),
                (
                  "old",
                  Json.obj(
                    ("audi", Json.fromString("a5"))
                  )
                )
              ),
              Json.obj(
                ("path", Json.fromString("cars[1]")),
                (
                  "old",
                  Json.obj(
                    ("renault", Json.fromString("clio"))
                  )
                )
              ),
              Json.obj(
                ("path", Json.fromString("nationality")),
                ("old", Json.fromString("German"))
              )
            )
          ),
          ("add", Json.arr())
        )
      }

      actual shouldBe expected
    }

  "Two complete json with nested objects" should
    "return a no differences Json" in {
      val entryJson1 =
        """{
        |  "name": "John",
        |  "surname": "Smith",
        |  "address": {
        |    "home" : "C/Here",
        |    "number" : "123",
        |    "location" : {
        |      "town" : "Malaga",
        |      "city" : "Malaga",
        |      "country" : "Spain"
        |    }
        |  }
        |}""".stripMargin

      val entryJson2 =
        """{
        |  "name": "Jack",
        |  "surname": "Smith",
        |  "address": {
        |    "home" : "C/Here",
        |    "number" : "123",
        |    "location" : {
        |      "town" : "Malaga",
        |      "city" : "Malaga",
        |      "country" : "Colombia"
        |    }
        |  }
        |}""".stripMargin

      val actual = buildSolutionWithDiffson(entryJson1, entryJson2)

      val expected = {
        Json.obj(
          (
            "change",
            Json.arr(
              Json.obj(
                ("path", Json.fromString("name")),
                ("new", Json.fromString("Jack")),
                ("old", Json.fromString("John"))
              ),
              Json.obj(
                ("path", Json.fromString("address.location.country")),
                ("new", Json.fromString("Colombia")),
                ("old", Json.fromString("Spain"))
              )
            )
          ),
          ("remove", Json.arr()),
          ("add", Json.arr())
        )
      }

      actual shouldBe expected
    }

}
