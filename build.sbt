import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "edu.rit.cs",
      scalaVersion := "2.12.8",
      crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.8"),
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "eson-scala",
    libraryDependencies += scalaTest % Test
  )
