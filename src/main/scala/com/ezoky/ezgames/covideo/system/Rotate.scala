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
trait Rotate[T]:
  extension (entity: T) def rotate: T

trait Rotates[I: Identifiable, D: Dimension]
  extends Games[I, D]:

  given Rotate[Game] with
    extension (entity: Game)
      override def rotate: Game =
        entity.copy(
          people = entity.people.map(_.rotate)
        )
