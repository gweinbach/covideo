package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.entity.Games

trait Accelerate[T]:
  extension (entity: T) def accelerate: T

trait Accelerates[I: Identifiable, D: Dimension]
  extends Games[I, D]:

  given Accelerate[Game] with
    extension (entity: Game)
      override def accelerate: Game =
        entity.copy(
          people = entity.people.map(_.accelerate)
        )



