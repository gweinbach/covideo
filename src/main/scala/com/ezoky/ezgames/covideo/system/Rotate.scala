/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Box, Position, Speed}
import com.ezoky.ezgames.covideo.entity.Game

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Rotate[T]:
  extension (entity: T) def rotate: T

given Rotate[Game] with
  extension (entity: Game)
    override def rotate: Game =
      entity.copy(
        people = entity.people.map(_.rotate)
      )
