package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.entity.Games

trait AngularAccelerate[T]:
  extension (entity: T) def angularAccelerate: T
  
trait AngularAccelerates[I: Identifiable, D: Dimension]
  extends Games[I, D]:

  given AngularAccelerate[Population[Person]] with
    extension (people: Population[Person])
      override def angularAccelerate: Population[Person] =
        people.map(_.angularAccelerate)

  given AngularAccelerate[Demography[Person]] with
    extension (demography: Demography[Person])
      override def angularAccelerate: Demography[Person] =
        demography.withPopulation(demography.population.angularAccelerate)

  given AngularAccelerate[Game] with
    extension (game: Game)
      override def angularAccelerate: Game =
        game.withDemography(
          game.demography.angularAccelerate
        )
        