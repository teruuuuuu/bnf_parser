import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "logging_converter",
    libraryDependencies += "com.github.kmizu" %% "scomb" % "0.9.0",
    libraryDependencies += "io.spray" %%  "spray-json" % "1.3.5",
    libraryDependencies += scalaTest % Test
  )

mainClass in (Compile, run) := Some("jp.co.teruuu.converter.BnfConverter")
