package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.Acceleration
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.entity.Game
import com.ezoky.ezgames.covideo.entity.People.{Person, Population}


trait Evolve[T]:
  extension (entity: Generated[T]) def evolve: Generated[T]


given Evolve[Person] with
  extension (entity: Generated[Person])
    override def evolve: Generated[Person] =
      for
        person <- entity
        within = person.accelerationRange
        newAcceleration <- Acceleration.generated(within, within, within)
      yield
        person.turn(newAcceleration)

given (using Evolve[Person]): Evolve[Game] with
  extension (entity: Generated[Game])
    override def evolve: Generated[Game] =
      for
        game <- entity
        evolvedPeople <- Generated.foldSetGen(game.people.values.toSet, _.evolve)
      yield
        game.copy(people = Population(evolvedPeople))