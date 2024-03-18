/*
 * @author gweinbach on 02/07/2022 15:34
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.{AllComponents, Dimension, Identifiable}

import spire.*
import spire.implicits.{*, given}
import spire.math.{*, given}

/**
 * @since 0.2.0
 * @author gweinbach on 02/07/2022
 */
trait Viewables[I: Identifiable, D: Dimension : Numeric]
  extends Entities[I]
    with AllComponents[D]
    with Games[I, D]
    with Worlds[I, D]:

  import CoordsDimension.*
  import CoordsDimension.Ez3D.*

  trait Viewable[T, V]:
    extension (viewable: T)
      def allViewables: Population[V]

  given [V, T](using Viewable[T, V]): Viewable[Population[T], V] with
    extension (population: Population[T])
      def allViewables: Population[V] =
        population.foldLeft(Population.empty[V]) {
          case (pop, (_, t)) =>
            pop ++ t.allViewables
        }

  given [V](using Viewable[World, V])(using Viewable[Person, V]): Viewable[Game, V] with
    extension (game: Game)
      override def allViewables: Population[V] =
        game.world.allViewables ++ game.people.allViewables


  trait ViewableSprite[T]
    extends Viewable[T, Sprite]

  given ViewableSprite[World] with
    extension (world: World)
      override def allViewables: Population[Sprite] =
        Population.empty

  given ViewableSprite[Person] with
    extension (person: Person)
      override def allViewables: Population[Sprite] =
        Population(
          person.id ->
            person.sprite
        )


  trait Viewable3D[T]
    extends Viewable[T, Component3D]

  private given Conversion[SizeValue, DimensionBase] with
    def apply(sizeValue: SizeValue): DimensionBase =
      sizeValue.baseValue

  private given Conversion[PositionValue, DimensionBase] with
    def apply(positionValue: PositionValue): DimensionBase =
      positionValue.baseValue

  /**
   * Very important point: coordinate base in World area is indirect, therefore z value is negated
   */
  private given Conversion[Position, SpacePoint] with
    def apply(position: Position): SpacePoint =
      SpacePoint(
        position.x.value,
        position.y.value,
        Zero-position.z.value
      )


  given Viewable3D[Person] with
    extension (person: Person)
      override def allViewables: Population[Component3D] =
        Population(
          person.id ->
            Component3D(
              position = person.solid.mobile.position,
              basis = person.solid.basis,
              shape = person.shape
            )
        )

  given Viewable3D[World] with
    extension (world: World)
      override def allViewables: Population[Component3D] =
        val boundaries =
          Parallelepiped(
            width = world.area.width.value,
            height = world.area.height.value,
            depth = world.area.depth.value
          )
        val origin =
          Cross.Uniform(Ten)
        Population(
          world.id -> Component3D(
            SpacePoint(boundaries.width / Two, boundaries.height / Two, -boundaries.depth / Two),
            Basis.NormalDirect,
            boundaries + origin
          )
        )
