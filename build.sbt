import Dependencies._

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

val circeVersion = "0.12.3"
val lift_json    = "net.liftweb" %% "lift-json" % "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "raytracer_challenge",
    libraryDependencies += scalaTest % Test,
    wartremoverErrors ++= Warts.allBut(Wart.Var,
                                       Wart.Overloading,
                                       Wart.OptionPartial,
                                       Wart.Any,
                                       Wart.Equals,
                                       Wart.Recursion,
                                       Wart.Throw,
                                       Wart.TraversableOps,
                                       Wart.AsInstanceOf),
    wartremoverWarnings ++= Warts.all,
    scalacOptions += "-Ypartial-unification",
    libraryDependencies += "io.circe"      %% "circe-yaml" % "0.10.0",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies += lift_json
  )

