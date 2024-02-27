package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.Identifiable

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

  
  opaque type Population[+E <: Entity] = Map[I, E]

  extension [A <: Entity](population: Population[A])

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

    def foldLeft[B](z: B)(op: (B, (I, A)) => B): B =
      Population.foldLeft(population)(z)(op)

  
  object Population:

    def apply[T <: Entity](people: Iterable[T]): Population[T] =
      people.foldLeft(empty) {
        case (map, person) =>
          map + (person.id -> person)
      }

    def apply[T <: Entity](people: (I, T)*): Population[T] =
      Map.from(people)

    def empty[T <: Entity]: Population[T] =
      Map.empty[I, T]

    def map[A <: Entity, B <: Entity](population: Population[A])(f: A => B): Population[B] =
      population.map {
        case (id, a) =>
          val b = f(a)
          (id, b)
      }

    def foldLeft[A <: Entity, B <: Entity](population: Population[A])(z: B)(op: (B, (I, A)) => B): B =
      population.foldLeft(z)(op)
