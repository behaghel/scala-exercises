name := """scala-exercises"""

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.2.0" % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.2.0" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
