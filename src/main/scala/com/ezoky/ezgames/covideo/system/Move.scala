/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.entity.Games

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Move[T]:
  extension(entity: T) def move: T

trait Moves[I: Identifiable, D: Dimension]
  extends Games[I, D]:
  
  given Move[Game] with
    extension(entity: Game)
      override def move: Game =
        entity.copy(
          people = entity.people.map(_.move)
        )
