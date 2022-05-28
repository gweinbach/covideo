package com.ezoky.ezgames.covideo

import com.ezoky.ezcategory.{Endomorphism, IO}
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.entity.Game

import scala.annotation.tailrec

case class GameLoopConfig(fps: Int)

class GameLoop(initialGame: Generated[Game],
               gameStep: Generated[Game] => Generated[IO[Game]],
               seed: Generator,
               gameLoopConfig: GameLoopConfig)
  extends Runnable :

  val stepDurationInNanoseconds = GameLoop.NanosecondsInOneSecond / gameLoopConfig.fps

  val thread = new Thread(this)
  thread.start()

  def run(): Unit =
    val nextStep = System.nanoTime() + stepDurationInNanoseconds
    loop(initialGame, seed, nextStep)

  @tailrec
  final def loop(game: Generated[Game],
                 generator: Generator,
                 nextStep: Long): Unit =

    val generatedGame: Generated[IO[Game]] = gameStep(game)
    val (ioGame, nextGen) = generatedGame(generator)
    val nextGame = Generated(ioGame.unsafeRun())

    val (remainingMilliseconds, remainingNanoseconds) =
      val remainingNs = nextStep - System.nanoTime()
      if (remainingNs < 0L)
        System.err.println(s"Overloaded: ${remainingNs}ns")
        (0L, 0)
      else
        (remainingNs / GameLoop.NanosecondsInOneMillisecond, (remainingNs % GameLoop.NanosecondsInOneMillisecond).intValue)
    Thread.sleep(remainingMilliseconds, remainingNanoseconds)

    loop(nextGame, nextGen, System.nanoTime() + stepDurationInNanoseconds)

object GameLoop:
  val NanosecondsInOneSecond = 1000000000L
  val NanosecondsInOneMillisecond = 1000000L