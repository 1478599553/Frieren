ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "Frieren",
    idePackagePrefix := Some("frieren")
  )
//libraryDependencies += "ch.usi.si.seart" % "java-tree-sitter" % "1.11.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"