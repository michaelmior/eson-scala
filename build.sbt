import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "edu.rit.cs",
      scalaVersion := "2.12.7",
      crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.7"),
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "eson-scala",
    libraryDependencies += scalaTest % Test
  )
