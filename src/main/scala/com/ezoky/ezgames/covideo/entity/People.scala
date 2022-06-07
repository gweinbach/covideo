/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.*

import java.util.UUID


/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
object People:

  case class Person(id: PersonId,
                    mobile: Mobile,
                    healthCondition: HealthCondition,
                    sprite: Sprite)
    extends Entity[PersonId]:

    def move: Person =
      val movedMobile = mobile.move
      copy(mobile = movedMobile, sprite = sprite.moveTo(movedMobile.position))

    def accelerate: Person =
      copy(mobile = mobile.accelerate)

    def turn(newAcceleration: Acceleration): Person =
      copy(mobile = mobile.turn(newAcceleration))


  opaque type PersonId = UUID

  object PersonId:
    def apply(): PersonId =
      UUID.randomUUID()


  opaque type Population[T] = Map[PersonId, T]

  object Population:

    def apply[T <: Entity[PersonId]](people: Iterable[T]): Population[T] =
      people.foldLeft(empty) {
        case (map, person) =>
          map + (person.id -> person)
      }

    def empty[T]: Population[T] =
      Map.empty[PersonId, T]

    def map[A, B](population: Population[A])(f: A => B): Population[B] =
      population.map {
        case (id, a) =>
          val b = f(a)
          (id, b)
      }

    def foldLeft[A,B](population: Population[A])(z: B)(op: (B, (PersonId, A)) => B): B =
      population.foldLeft(z)(op)

  extension [A](population: Population[A])

    def values: Iterable[A] =
      population.values

    def add(kv: (PersonId, A)): Population[A] =
      population + kv

    def map[B](f: A => B): Population[B] =
      Population.map(population)(f)

    def foldLeft[B](z: B)(op: (B, (PersonId, A)) => B): B =
      Population.foldLeft(population)(z)(op)



  case class PersonConfig(mobileConfig: MobileConfig)
