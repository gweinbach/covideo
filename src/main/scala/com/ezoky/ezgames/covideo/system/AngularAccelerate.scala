package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.entity.Game

trait AngularAccelerate[T]:
  extension (entity: T) def angularAccelerate: T

given AngularAccelerate[Game] with
  extension (entity: Game)
    override def angularAccelerate: Game =
      entity.copy(
        people = entity.people.map(_.angularAccelerate)
      )