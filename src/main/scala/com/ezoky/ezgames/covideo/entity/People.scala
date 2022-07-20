/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*
import com.ezoky.ezgames.covideo.component.Generate.{Generated, generatedBetweenFractional}

import spire.*
import spire.math.*
import spire.implicits.*

import java.util.UUID
import scala.annotation.targetName


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
      
    def rotate: Person =
      copy(solid = solid.rotate)

    def angularAccelerate: Person =
      copy(solid = solid.angularAccelerate)

    def withSolid(solid: Solid): Person =
      copy(solid = solid)

  case class PersonConfig(shape: Generated[Shape],
                          solidConfig: SolidConfig)

  object PersonId:
    def apply(): PersonId =
      UUID.randomUUID()

  extension[A] (population: Population[A])

    @targetName("populationSize")
    def number: Int =
      population.size

    def values: Iterable[A] =
      population.values

    @targetName("add")
    infix def +(kv: (PersonId, A)): Population[A] =
      population + kv

    @targetName("populationMerge")
    infix def ++(other: Population[A]): Population[A] =
      population ++ other

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

    def apply[T](people: (PersonId, T)*): Population[T] =
      Map.from(people)

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
