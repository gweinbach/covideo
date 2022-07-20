package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Acceleration, AccelerationRange, AngularAcceleration, AngularAccelerationRange, Mobile, Solid}
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.entity.Game
import com.ezoky.ezgames.covideo.entity.People.{Person, Population}


trait Evolve[T]:
  extension (generatedEntity: Generated[T]) def evolve: Generated[T]


given Evolve[Mobile] with
  extension (generatedMobile: Generated[Mobile])
    override def evolve: Generated[Mobile] =
      for
        mobile <- generatedMobile
        within = mobile.accelerationRange
        newAcceleration <- Acceleration.generated(within, within, within)
      yield
        mobile.turn(newAcceleration)

given (using Evolve[Mobile]): Evolve[Solid] with
  extension (generatedSolid: Generated[Solid])
    override def evolve: Generated[Solid] =
      for
        solid <- generatedSolid
        evolvedMobile <- Generated(solid.mobile).evolve
        within = solid.angularAccelerationRange
        newAccelerationRange <- AngularAcceleration.generated(within, within, within)
      yield
        solid.twirl(newAccelerationRange).withMobile(evolvedMobile)


given (using Evolve[Solid]): Evolve[Person] with
  extension (generatedPerson: Generated[Person])
    override def evolve: Generated[Person] =
      for
        person <- generatedPerson
        evolvedSolid <- Generated(person.solid).evolve
      yield
        person.withSolid(solid = evolvedSolid)

given (using Evolve[Person]): Evolve[Game] with
  extension (generatedGame: Generated[Game])
    override def evolve: Generated[Game] =
      for
        game <- generatedGame
        evolvedPeople <- Generated.flatMapSet(game.people.values.toSet, _.evolve)
      yield
        game.withPeople(people = Population(evolvedPeople))