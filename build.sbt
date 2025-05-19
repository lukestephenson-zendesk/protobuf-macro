ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

ThisBuild / scalacOptions ++= Seq(
  "-Xprint:postInlining",
  "-Xmax-inlines:100000"
)

lazy val root = (project in file("."))
  .settings(
    name := "protobuf-macro"
  )

val kyoVersion = "0.16.2"

libraryDependencies += "io.getkyo" %% "kyo-data"          % kyoVersion
libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"