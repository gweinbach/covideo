package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.entity.{Game, Person}

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
      entity.flatMap(
        game =>
          (seed: Generator) =>
            val evolvedPeople =
              game.people.foldLeft((Set.empty[Person], seed)) {
                case ((setOfPersons, gen), person) =>
                  val (evolvedPerson, nextGen) = Generated.unit(person).evolve(gen)
                  (setOfPersons + evolvedPerson, nextGen)
              }
            (game.copy(people = evolvedPeople._1), evolvedPeople._2)
      )



