organization := "io.getnelson.nelson"

scalaVersion := "3.4.3"

lazy val nelson = project.in(file(".")).aggregate(api, core, http)

lazy val api = project

lazy val core = project.dependsOn(api)

lazy val http = project.dependsOn(core % "test->test;compile->compile")

// Disable publishing for the root project
publish / skip := true

addCommandAlias("ci", ";test")
