package com.ezoky.ezgames.covideo

import com.ezoky.ezcategory.{Endomorphism, IO}
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.Dimension.{given, *}
import com.ezoky.ezgames.covideo.entity.*
import com.ezoky.ezgames.covideo.system.{Accelerate, AngularAccelerate, Display, DisplaySystem, Evolve, Move, Rotate, given}
import com.ezoky.ezgames.covideo.system.swing.*

import scala.annotation.tailrec

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */

def msg = s"I was compiled by scala 3 but using scala ${util.Properties.versionNumberString} stdlib :)"

given DisplaySystem = SwingDisplaySystem

@main def main: Unit =
  println(msg)

  val generator = new RandomGenerator()
  val game = GameBuilder(Config.Game).build

  new GameLoop(game, step, generator, Config.Loop)


def step(game: Generated[Game]): Generated[IO[Game]] =
  for
    nextGame <- game.evolve
  yield
    nextGame.move.accelerate.rotate.angularAccelerate.display
