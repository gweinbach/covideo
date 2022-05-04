/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Area, Position, Speed}
import com.ezoky.ezgames.covideo.entity.{Game, Person, World}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Move[T]:
  extension(entity: T) def move(within: Area): T

given Move[Person] with
  extension(entity: Person)
    override def move(within: Area): Person =
      entity.move(within)

given (using Move[Person]): Move[Game] with
  extension(entity: Game)
    override def move(within: Area): Game =
      entity.copy(
        people = entity.people.map(_.move(within))
      )
