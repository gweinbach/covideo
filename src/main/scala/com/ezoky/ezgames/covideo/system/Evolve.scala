package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable, Mobiles, Solids}
import com.ezoky.ezgames.covideo.entity.{Games, Persons, Scenes, Viewables, Worlds}


trait Evolve[T]:
  extension (generatedEntity: Generated[T]) def evolve: Generated[T]

trait Evolves[I: Identifiable, D: Dimension]
  extends Games[I, D]
    with Worlds[I, D]
    with Scenes[I, D]
    with Persons[I, D]
    with Viewables[I, D]
    with Mobiles[D]
    with Solids[D]:

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

          // We get all sprites
          sprites: Population[Sprite] = game.allViewables

          // We get all 3D components
          components: Population[Component3D] = game.allViewables

          evolvedWorld = game.world.withSprites(sprites).withComponents(components)

        yield
          game.withPeople(people = Population(evolvedPeople)).withWorld(evolvedWorld)
          