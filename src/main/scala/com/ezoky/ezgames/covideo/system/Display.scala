package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.Sprite
import com.ezoky.ezgames.covideo.entity.{Game, Scene}

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Display[T]:
  extension(entity: T) def display: T

given Display[Game] with
  extension (entity: Game)
    override def display: Game =
      entity.copy(
        world = entity.world.copy(
          scene = entity.world.scene.withSprites(entity.people.map(_.sprite))
        )
      )
