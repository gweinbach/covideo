package com.ezoky.ezgames.covideo

import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.double.DoubleDimension
import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable, UUIDIdentifiable}

import spire.implicits.*

import java.util.UUID

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */

def msg = s"I was compiled by scala 3 but using scala ${util.Properties.versionNumberString} stdlib :)"

object Main:

  given Dimension[Double] = DoubleDimension

  given Identifiable[UUID] = UUIDIdentifiable

  object Everything
    extends GameBootstrap[UUID, Double]
  
  import Everything.DisplaySystem

  given DisplaySystem = Everything.SwingDisplaySystem

import Main.{*, given}
import Main.Everything.{*, given}
//import Main.Everything.CoordsDimension.{*, given}

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
