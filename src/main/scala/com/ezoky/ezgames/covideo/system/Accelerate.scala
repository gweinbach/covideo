package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.entity.{Game, Person}

trait Accelerate[T]:
  extension (entity: T) def accelerate(within: AccelerationRange): T


given Accelerate[Generated[Person]] with
  extension (entity: Generated[Person])
    override def accelerate(within: AccelerationRange): Generated[Person] =
      for
        acceleration <- Acceleration.generated(within, within, within)
        person <- entity
      yield
        println(person)
        person.copy(speed = acceleration.accelerate(person.speed))

given (using Accelerate[Generated[Person]]): Accelerate[Generated[Game]] with
  extension (entity: Generated[Game])
    override def accelerate(within: AccelerationRange): Generated[Game] =
      entity.flatMap(
        game =>
          (seed: Generator) =>
            val acceleratedPeople =
              game.people.foldLeft((Set.empty[Person], seed)) {
                case ((setOfPersons, gen), person) =>
                  val (acceleratedPerson, nextGen) = Generated.unit(person).accelerate(within)(gen)
                  (setOfPersons + acceleratedPerson, nextGen)
              }
            (game.copy(people = acceleratedPeople._1), acceleratedPeople._2)
      )



