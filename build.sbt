organization := "com.github.mpilquist"
name := "simulacrum"

scalaVersion := "2.11.5"
crossScalaVersions := Seq("2.11.5")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  "org.scalatest" %% "scalatest" % "2.2.3" % "test"
)
addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
