package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.entity.Games

trait AngularAccelerate[T]:
  extension (entity: T) def angularAccelerate: T
  
trait AngularAccelerates[I: Identifiable, D: Dimension]
  extends Games[I, D]:
  
  given AngularAccelerate[Game] with
    extension (entity: Game)
      override def angularAccelerate: Game =
        entity.copy(
          people = entity.people.map(_.angularAccelerate)
        )