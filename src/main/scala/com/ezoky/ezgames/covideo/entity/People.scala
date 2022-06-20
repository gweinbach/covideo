/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*

import java.util.UUID


/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
object People:

  opaque type PersonId = UUID
  opaque type Population[T] = Map[PersonId, T]

  case class Person(id: PersonId,
                    solid: Solid,
                    healthCondition: HealthCondition,
                    sprite: Sprite,
                    shape: Shape)
    extends Entity[PersonId] :

    def move: Person =
      val movedSolid = solid.move
      copy(
        solid = movedSolid,
        sprite = sprite.moveTo(solid.mobile.position)
      )

    def accelerate: Person =
      copy(solid = solid.accelerate)

    def turn(newAcceleration: Acceleration): Person =
      copy(solid = solid.turn(newAcceleration))

  case class PersonConfig(mobileConfig: MobileConfig)

  object PersonId:
    def apply(): PersonId =
      UUID.randomUUID()

  extension[A] (population: Population[A])

    def values: Iterable[A] =
      population.values

    def add(kv: (PersonId, A)): Population[A] =
      population + kv

    def map[B](f: A => B): Population[B] =
      Population.map(population)(f)

    def foldLeft[B](z: B)(op: (B, (PersonId, A)) => B): B =
      Population.foldLeft(population)(z)(op)

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

    def foldLeft[A, B](population: Population[A])(z: B)(op: (B, (PersonId, A)) => B): B =
      population.foldLeft(z)(op)
