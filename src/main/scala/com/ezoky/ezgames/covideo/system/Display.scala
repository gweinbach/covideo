package com.ezoky.ezgames.covideo.system

import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.entity.{Game, Scene, SceneDimension}

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Display[T]:
  extension (entity: T) def display: IO[T]

given (using DisplaySystem): Display[Game] with
  extension (entity: Game)
    override def display: IO[Game] =
      val scene = entity.world.scene
      val sprites = entity.people.map(_.sprite)
      val updatedScene = scene.withSprites(sprites)

      for
        _ <- summon[DisplaySystem].drawScene(updatedScene)
      yield
        entity.copy(
          world = entity.world.copy(
            scene = updatedScene
          )
        )

trait DisplaySystem:

  def defaultScreenSceneDimension: SceneDimension
  def drawScene(scene: Scene): IO[Unit]