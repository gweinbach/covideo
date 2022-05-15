package com.ezoky.ezgames.covideo

import com.ezoky.ezcategory.Endomorphism
import com.ezoky.ezgames.covideo.component.{Generated, Generator}
import com.ezoky.ezgames.covideo.entity.Game

import scala.annotation.tailrec

case class GameLoopConfig(fps: Int)

class GameLoop(initialGame: Generated[Game],
               gameStep: Endomorphism[(Generated[Game], Generator)],
               seed: Generator,
               gameLoopConfig: GameLoopConfig)
  extends Runnable :

  val stepDurationInNanoseconds = GameLoop.NanosecondsInSecond / gameLoopConfig.fps

  val thread = new Thread(this)
  thread.start()

  def run(): Unit =
    val nextStep = System.nanoTime() + stepDurationInNanoseconds
    loop(initialGame, seed, nextStep)

  @tailrec
  final def loop(generatedGame: Generated[Game],
                 generator: Generator,
                 nextStep: Long): Unit =

    val (nextGame, nextGen) = gameStep((generatedGame, generator))

    val (remainingMilliseconds, remainingNanoseconds) =
      val remainingNs = nextStep - System.nanoTime()
      if (remainingNs < 0L)
        System.err.println(s"Overloaded: ${remainingNs}ns")
        (0L, 0)
      else
        (remainingNs / GameLoop.NanosecondsInMillisecond, (remainingNs % GameLoop.NanosecondsInMillisecond).intValue)
    Thread.sleep(remainingMilliseconds, remainingNanoseconds)

    loop(nextGame, nextGen, System.nanoTime() + stepDurationInNanoseconds)

object GameLoop:
  val NanosecondsInSecond = 1000000000L
  val NanosecondsInMillisecond = 1000000L