package com.ezoky.ezgames.covideo

import com.ezoky.ezcategory.Endomorphism
import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.entity.*
import com.ezoky.ezgames.covideo.system.given
import com.ezoky.ezgames.covideo.system.swing.*

import scala.annotation.tailrec

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */

def msg = s"I was compiled by scala 3 but using scala ${util.Properties.versionNumberString} stdlib :)"

val three: 3 = 3

def intMethod(i: Int): Int =
  i * three

def unionTypeMethod(sthg: Int | String): String =
  sthg match
    case i: Int => (3 * i).toString
    case s: String => s"Error: $s"


@main def main: Unit =
  val generator = new RandomGenerator()
  val game = GameBuilder(Config.Game).build

  new GameLoop(game, step, generator, Config.Loop)



def step(generatedGame: Generated[Game],
         generator: Generator): (Generated[Game], Generator) =
  val (evolvedGame, nextGen) = generatedGame.evolve(generator)
  val nextGame = Generated.unit(evolvedGame.move(within = evolvedGame.world.area).accelerate.display)
  (nextGame, nextGen)