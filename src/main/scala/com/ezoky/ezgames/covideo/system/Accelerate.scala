package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.entity.{Game, Person}

trait Accelerate[T]:
  extension (entity: T) def accelerate: T

given Accelerate[Game] with
  extension (entity: Game)
    override def accelerate: Game =
      entity.copy(
        people = entity.people.map(_.accelerate)
      )



