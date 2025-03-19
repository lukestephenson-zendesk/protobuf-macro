ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

ThisBuild / scalacOptions ++= Seq(
  "-Xprint:postInlining",
  "-Xmax-inlines:100000"
)

lazy val root = (project in file("."))
  .settings(
    name := "protobuf-macro"
  )
