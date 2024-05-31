import io.circe.Json
import jsonDiff.jsonDiff.buildJsonDiffSolution
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

    val actual = buildJsonDiffSolution(entryJson1, entryJson2)

    val expected = {
      Json.obj(("first__eq", Json.fromString("second")))
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

    val actual = buildJsonDiffSolution(entryJson1, entryJson2)

    val expected = {
      Json.obj(("first__eq", Json.arr(Json.fromString("second"))))
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

    val actual = buildJsonDiffSolution(entryJson1, entryJson2)

    val expected = {
      Json.obj(("first__eq", Json.obj(("second", Json.fromString("zubehor")))))
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

    val actual = buildJsonDiffSolution(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("first_new", Json.fromString("third")),
        ("first_old", Json.fromString("second"))
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

    val actual = buildJsonDiffSolution(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("first_new", Json.arr(Json.fromString("third"))),
        ("first_old", Json.arr(Json.fromString("second")))
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

    val actual = buildJsonDiffSolution(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("first_new", Json.obj(("second", Json.fromString("bye")))),
        ("first_old", Json.obj(("second", Json.fromString("hello"))))
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

    val actual = buildJsonDiffSolution(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("first_new", Json.Null),
        ("first_old", Json.fromString("hello")),
        ("second_new", Json.obj(("second", Json.fromString("bye")))),
        ("second_old", Json.obj(("second", Json.fromString("hello"))))
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

    val actual = buildJsonDiffSolution(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("name_new", Json.fromString("Jack")),
        ("name_old", Json.fromString("John")),
        ("surname__eq", Json.fromString("Smith")),
        ("cars_new", Json.arr(
          Json.obj(("audi", Json.fromString("a1"))),
          Json.obj(("ford", Json.fromString("focus")))
        )),
        ("cars_old", Json.arr(
          Json.obj(("audi", Json.fromString("a1"))),
          Json.obj(("renault", Json.fromString("clio"))),
          Json.obj(("audi", Json.fromString("a5"))),
          Json.obj(("ford", Json.fromString("focus")))
        )),
        ("nationality_new", Json.Null),
        ("nationality_old", Json.fromString("German"))
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

    val actual = buildJsonDiffSolution(entryJson1, entryJson2)

    val expected = {
      Json.obj(
        ("name_new", Json.fromString("Jack")),
        ("name_old", Json.fromString("John")),
        ("surname__eq", Json.fromString("Smith")),
        ("address_new", Json.obj(
          ("location", Json.obj(("country", Json.fromString("Colombia"))))
        )),
        ("address_old", Json.obj(
          ("home", Json.fromString("C/Here")),
          ("number", Json.fromString("123")),
          ("location", Json.obj(
            ("town", Json.fromString("Malaga")),
            ("city", Json.fromString("Malaga")),
            ("country", Json.fromString("Spain"))
          ))
        ))
      )
    }

    actual shouldBe expected
  }

}
