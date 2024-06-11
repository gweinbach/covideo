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

  given Move[Population[Person]] with
    extension (people: Population[Person])
      override def move: Population[Person] =
        people.map(_.move)
        
  given Move[Game] with
    extension(game: Game)
      override def move: Game =
        game.withPeople(
          game.people.move
        )
