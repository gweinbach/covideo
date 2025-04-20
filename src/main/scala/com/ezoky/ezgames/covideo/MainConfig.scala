package com.ezoky.ezgames.covideo

import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.double.DoubleDimension
import com.ezoky.ezgames.covideo.component.float.FloatDimension
import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable, UUIDIdentifiable}
import spire.implicits.*

import java.util.UUID

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */

def msg = s"I was compiled by scala 3 but using scala ${util.Properties.versionNumberString} stdlib :)"

trait DoubleConfig:

  given Dimension[Double] = DoubleDimension

  given Identifiable[UUID] = UUIDIdentifiable

  object Everything
    extends SwingGameBootstrap[UUID, Double]
//      extends JavaFXGameBootstrap[UUID, Double]


trait FloatConfig:

  given Dimension[Float] = FloatDimension

  given Identifiable[UUID] = UUIDIdentifiable

  object Everything
    extends SwingGameBootstrap[UUID, Float]
  //    extends JavaFXGameBootstrap[UUID, Float]


object MainConfig extends DoubleConfig


@main def main: Unit =
  println(msg)

  import com.ezoky.ezgames.covideo.MainConfig.Everything.{*, given}
  import com.ezoky.ezgames.covideo.MainConfig.{*, given}

  given DisplaySystem = Everything.displaySystem(Config.UserControl)

  val generator = new RandomGenerator()
  val game = GameBuilder(Config.Game).build

  new GameLoop(game, step, generator, Config.Loop).start()

  def step(game: Generated[Game]): Generated[IO[Game]] =
    for
      nextGame <- game.beBornAndDie.evolve
    yield
//      nextGame.move.rotate.display
      nextGame.move.accelerate.rotate.angularAccelerate.display
