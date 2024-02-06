ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "Frieren",
    idePackagePrefix := Some("frieren")
  )
resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies += "com.github.serenadeai" % "java-tree-sitter" % "1.1.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"