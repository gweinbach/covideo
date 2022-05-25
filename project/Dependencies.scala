import sbt._

/**
 * @author gweinbach on 29/12/2021
 * @since 0.2.0
 */
object Dependencies {

  object Versions {

//    val scala211 = "2.11.12"
//    val scala212 = "2.12.15"
    val scala213 = "2.13.7"

    val scala2 = scala213
    val scala3 = "3.1.1"

//    val Cats = "2.7.0"
//
//    // Cats
//    val `cats-kernel` = "org.typelevel" %% "cats-kernel" % Versions.Cats
//    val `cats-core` = "org.typelevel" %% "cats-core" % Versions.Cats
//    val `cats-mtl` = "org.typelevel" %% "cats-mtl" % Versions.Cats
//    // Minimal dependencies to use cats library
//    val `cats-minimal` = Seq(`cats-kernel`, `cats-core`)

    object Test {

//      val SLF4J = "1.7.30"
//      val Logback = "1.2.3"

      val JunitInterface = "0.11"
      val Scalatest = "3.2.10"
      val Scalacheck = "1.15.4"
    }
  }
}
