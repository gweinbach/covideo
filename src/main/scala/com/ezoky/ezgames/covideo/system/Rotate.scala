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

  given Rotate[Population[Person]] with
    extension (people: Population[Person])
      override def rotate: Population[Person] =
        people.map(_.rotate)

  given Rotate[Demography[Person]] with
    extension (demography: Demography[Person])
      override def rotate: Demography[Person] =
        demography.withPopulation(
          demography.population.rotate
        )

  given Rotate[Game] with
    extension (game: Game)
      override def rotate: Game =
        game.withDemography(
          game.demography.rotate
        )
