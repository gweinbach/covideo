package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Acceleration, Mobile, Solid}
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.entity.Game
import com.ezoky.ezgames.covideo.entity.People.{Person, Population}


trait Evolve[T]:
  extension (entity: Generated[T]) def evolve: Generated[T]


given Evolve[Mobile] with
  extension (entity: Generated[Mobile])
    override def evolve: Generated[Mobile] =
      for
        mobile <- entity
        within = mobile.accelerationRange
        newAcceleration <- Acceleration.generated(within, within, within)
      yield
        mobile.turn(newAcceleration)

given (using Evolve[Mobile]): Evolve[Solid] with
  extension (entity: Generated[Solid])
    override def evolve: Generated[Solid] =
      for
        solid <- entity
        evolvedMobile <- Generated(solid.mobile).evolve
      yield
        solid.copy(mobile = evolvedMobile)


given (using Evolve[Solid]): Evolve[Person] with
  extension (entity: Generated[Person])
    override def evolve: Generated[Person] =
      for
        person <- entity
        evolvedSolid <- Generated(person.solid).evolve
      yield
        person.copy(solid = evolvedSolid)

given (using Evolve[Person]): Evolve[Game] with
  extension (entity: Generated[Game])
    override def evolve: Generated[Game] =
      for
        game <- entity
        evolvedPeople <- Generated.flatMapSet(game.people.values.toSet, _.evolve)
      yield
        game.copy(people = Population(evolvedPeople))