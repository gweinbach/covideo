package com.ezoky.ezgames.covideo

import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.RandomGenerator
import com.ezoky.ezgames.covideo.component.{AccelerationRange, Generated, Generator, Speed, SpeedRange, XSpeed, YSpeed, ZSpeed}
import com.ezoky.ezgames.covideo.entity.Game
import com.ezoky.ezgames.covideo.system.swing.*

import scala.annotation.tailrec

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */

def msg = s"I was compiled by scala 3 but using scala ${util.Properties.versionNumberString} stdlib :)"

val three: 3 = 3

def intMethod(i: Int): Int =
  i * 3

def unionTypeMethod(sthg: Int | String): String =
  sthg match
    case i: Int => (3 * i).toString
    case s: String => s"Error: $s"


object Config:
  val Area: WorldConfig = WorldConfig(Geometry.Bounded, 50 px, 50 px, 50 px, 50 px)
  val Person: PersonConfig = PersonConfig(
    initialSpeedRange = SpeedRange(-10.0 speed, 10.0 speed), 
    accelerationRange = AccelerationRange(-0.1 acceleration, 0.1 acceleration)
  )
  val Game: GameConfig = GameConfig(1, Person, Area)


@main def main: Unit =
  println("Hello world!")
  println(msg)
  println(intMethod(three))
  println(unionTypeMethod(three))
  println(unionTypeMethod("bad value"))

  val generator = new RandomGenerator()

  val (game, gen) = GameBuilder(Config.Game).build(generator)
  loop(game, gen, Config.Game)


import com.ezoky.ezgames.covideo.system.given

@tailrec
def loop(game: Game,
         generator: Generator,
         gameConfig: GameConfig): Unit =
  val nextGame = game.display.move(within = game.world.area)
  val (acceleratedGame, nextGen) = Generated.unit(nextGame).accelerate(gameConfig.personConfig.accelerationRange)(generator)
  loop(acceleratedGame, nextGen, gameConfig)


