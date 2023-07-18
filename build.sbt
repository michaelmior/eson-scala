import Dependencies._


lazy val commonSettings = Seq(
    libraryDependencies += scalaTest % Test
)

lazy val root = (project in file(".")).
  settings(
    commonSettings,
    inThisBuild(List(
      organization := "edu.rit.cs",
      scalaVersion := "2.13.11",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "eson-scala",
  ).dependsOn(dependencies)
   .aggregate(dependencies, metanome)

lazy val dependencies = (project in file("dependencies")).
  settings(commonSettings)

lazy val metanome = (project in file("metanome")).
  settings(
    commonSettings,
    resolvers += "michaelmior-Metanome" at "https://packagecloud.io/michaelmior/Metanome/maven2",
    libraryDependencies += "de.metanome" % "algorithm_integration" % "1.2-calcite-SNAPSHOT"
  ).dependsOn(dependencies)
