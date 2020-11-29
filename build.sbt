import Dependencies._

ThisBuild / scalaVersion     := "2.12.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "jp.co.teruuu"
ThisBuild / organizationName := "teruuu"

libraryDependencies ++= Seq(
  "com.github.kmizu" %% "scomb" % "0.9.0",
  "io.spray" %%  "spray-json" % "1.3.5",
  "org.scalatest" %% "scalatest" % "3.2.3" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.1" % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "bnf_parser"
  )

mainClass in (Compile, run) := Some("jp.co.teruuu.bnf.Converter")
