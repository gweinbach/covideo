package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.entity.*

trait Accelerate[T]:
  extension (entity: T) def accelerate: T

given Accelerate[Game] with
  extension (entity: Game)
    override def accelerate: Game =
      entity.copy(
        people = entity.people.mapEntity(_.accelerate)
      )



