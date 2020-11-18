
Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / organization := "com.ezoky"
ThisBuild / licenses := Seq("Apache 2.0 License" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

val dottyVersion = "3.0.0-M1"

ThisBuild / scalacOptions ++= Seq(
//  "-Yrangepos", // use range positions for syntax trees
  "-language:postfixOps", //  enables postfix operators
//  "-language:implicitConversions", // enables defining implicit methods and members
//  "-language:existentials", // enables writing existential types
//  "-language:reflectiveCalls", // enables reflection
//  "-language:higherKinds", // allow higher kinded types without `import scala.language.higherKinds`
  "-encoding", "UTF-8", // source files are in UTF-8
  "-deprecation", // warns about use of deprecated APIs
//  "-Wunused:imports", // warns on unused imports
//  "-unchecked", // warns about unchecked type parameters
  "-feature", // warns about misused language features
//  "-Xlint", // enables handy linter warnings
//  "-Xfatal-warnings", // turns compiler warnings into errors
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "covideo",
    version := "0.1.0",

    scalaVersion := dottyVersion,
//    scalaVersion := dottyLatestNightlyBuild.get,
    
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += ("org.scalacheck" %% "scalacheck" % "1.14.3" % "test").withDottyCompat(scalaVersion.value),
    libraryDependencies += ("org.scalatest" %% "scalatest" % "3.2.0" % "test").withDottyCompat(scalaVersion.value)
  )
