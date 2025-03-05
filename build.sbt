ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

lazy val root = (project in file("."))
  .settings(
    name := "protobuf-macro"
  )

libraryDependencies += "io.getkyo" %% "kyo-data"          % "0.16.2"