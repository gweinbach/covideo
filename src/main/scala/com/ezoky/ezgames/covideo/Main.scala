package com.ezoky.ezgames.covideo

import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.{Movement, SpeedRange, XSpeed, YSpeed, ZSpeed}
import com.ezoky.ezgames.covideo.system.swing.GameBuilder
import com.ezoky.ezgames.covideo.system.{Display, Move}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */

def msg = s"I was compiled by scala 3 but using scala ${util.Properties.versionNumberString} stdlib :)"

val three: 3 = 3

def intMethod(i: Int): Int =
  i * 3

def unionTypeMethod(sthg: Int | String): String =
  sthg match {
    case i: Int => (3 * i).toString
    case s: String => s"Error: $s"
  }

@main def main: Unit = {
  println("Hello world!")
  println(msg)
  println(intMethod(three))
  println(unionTypeMethod(three))
  println(unionTypeMethod("bad value"))

  val game = GameBuilder()(using Geometry.Bounded).build
  Display(game.world.scene, game.people.map(_.sprite)).evolve

  val speedRange = SpeedRange(0.0 speed, 10.0 speed)
  val move = Movement(
    XSpeed.RandomWithin(speedRange),
    YSpeed.RandomWithin(speedRange),
    ZSpeed.Zero,
  )

//  Move()
}


