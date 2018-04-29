name := "di-experiments"

version := "0.1"

scalaVersion := "2.12.5"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4",
  "org.typelevel" %% "cats-core" % "1.1.0"
)