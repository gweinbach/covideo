package com.ezoky.ezgames.covideo

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
object Main {

  val three: 3 = 3

  def intMethod(i: Int): Int =
    i * 3

  def unionTypeMethod(sthg: Int | String): String =
    sthg match {
      case i: Int => (3 * i).toString
      case s: String => s"Error: $s"
    }

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    println(msg)
    println(intMethod(three))
    println(unionTypeMethod(three))
    println(unionTypeMethod("bad value"))
  }

  def msg = "I was compiled by scala 3 :)"

}
