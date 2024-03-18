package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.Identifiable

import scala.annotation.targetName

trait Entities[I: Identifiable]:
  
  trait Entity:
    val id: I
  
    override def equals(obj: Any): Boolean =
      obj match
        case that: Entity =>
          (that != null) && (that.id == this.id)
        case default =>
          false
     
    override def hashCode(): Int =
      id.hashCode()

  
  opaque type Population[+E] = Map[I, E]

  /**
   * Things are easy when the Population is populated with Entities
   */
  extension [A <: Entity](population: Population[A])

    @targetName("addEntity")
    infix def +(v: A): Population[A] =
      population + (v.id -> v)
    
  
  extension [A](population: Population[A])

    @targetName("populationSize")
    def number: Int =
      population.size

    def values: Iterable[A] =
      population.values

    @targetName("add")
    infix def +(kv: (I, A)): Population[A] =
      population + kv

    @targetName("populationMerge")
    infix def ++(other: Population[A]): Population[A] =
      population ++ other

    def map[B](f: A => B): Population[B] =
      Population.map(population)(f)

    def foldLeft[B](z: B)(op: (B, (I, A)) => B): B =
      Population.foldLeft(population)(z)(op)

  
  object Population:

    def apply[T <: Entity](entities: Iterable[T]): Population[T] =
      entities.foldLeft(empty) {
        case (map, entity) =>
          map + (entity.id -> entity)
      }

    def apply[T](keyValues: (I, T)*): Population[T] =
      Map.from(keyValues)

    def empty[T]: Population[T] =
      Map.empty[I, T]

    def map[A, B](population: Population[A])(f: A => B): Population[B] =
      population.map {
        case (id, a) =>
          val b = f(a)
          (id, b)
      }

    def foldLeft[A, B](population: Population[A])(z: B)(op: (B, (I, A)) => B): B =
      population.foldLeft(z)(op)
