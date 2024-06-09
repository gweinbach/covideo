package com.ezoky.ezgames.covideo

import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.Generate.*

import scala.annotation.tailrec
import MainConfig.{*, given}
import MainConfig.Everything.{*, given}

case class GameLoopConfig(fps: Int)

class GameLoop(initialGame: Generated[Game],
               gameStep: Generated[Game] => Generated[IO[Game]],
               seed: Generator,
               gameLoopConfig: GameLoopConfig):

  val stepDurationInNanoseconds = GameLoop.NanosecondsInOneSecond / gameLoopConfig.fps

  final def start(): Unit =
    val nextStep = System.nanoTime() + stepDurationInNanoseconds
    loop(initialGame, seed, nextStep)

  @tailrec
  final def loop(game: Generated[Game],
                 generator: Generator,
                 nextStep: Long): Unit =

    // what should be done during next step of the game
    val generatedIOGame: Generated[IO[Game]] = gameStep(game)

    // Let's get out of the monads
    val (ioGame: IO[Game], nextGen) = generatedIOGame(generator)
    val nextGame: Generated[Game] = Generated(ioGame.unsafeRun())

    val (remainingMilliseconds, remainingNanoseconds) =
      val remainingNs = nextStep - System.nanoTime()
      if (remainingNs < 0L)
        System.err.println(s"Overloaded: ${remainingNs}ns")
        (0L, 0)
      else
        (remainingNs / GameLoop.NanosecondsInOneMillisecond, (remainingNs % GameLoop.NanosecondsInOneMillisecond).intValue)
    Thread.sleep(remainingMilliseconds, remainingNanoseconds)

    if (nextGame.get(generator).status == GameStatus.Running) then
      loop(nextGame, nextGen, System.nanoTime() + stepDurationInNanoseconds)

object GameLoop:
  val NanosecondsInOneSecond = 1000000000L
  val NanosecondsInOneMillisecond = 1000000L