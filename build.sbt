ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "json-fun"
  )

libraryDependencies ++= Seq(
  "io.circe" %% "circe-generic" % "0.14.7",
  "io.circe" %% "circe-core" % "0.14.7",
  "io.circe" %% "circe-parser" % "0.14.7",

  "org.json4s" %% "json4s-core" % "4.0.7",
  "org.json4s" %% "json4s-native" % "4.0.7",

  "org.gnieh" %% "diffson-circe" % "4.6.0",

  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
)