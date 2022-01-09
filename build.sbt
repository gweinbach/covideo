import Dependencies.Versions

Global / onChangedBuildSource := ReloadOnSourceChanges

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
    version := "0.1.0-SNAPSHOT",

    scalaVersion := Versions.scala3,

    libraryDependencies += "com.novocode" % "junit-interface" % Versions.Test.JunitInterface % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % Versions.Test.Scalacheck % "test",
//    libraryDependencies += "org.scalatest" %% "scalatest" % Versions.Test.Scalatest % "test"
  )
