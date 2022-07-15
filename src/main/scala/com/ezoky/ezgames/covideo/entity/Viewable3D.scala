/*
 * @author gweinbach on 02/07/2022 15:34
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*
import com.ezoky.ezgames.covideo.component.Position
import com.ezoky.ezgames.covideo.entity.People.*

/**
 * @since 0.2.0
 * @author gweinbach on 02/07/2022
 */
trait Viewable3D[T]:
  extension (viewable: T)
    def all3DComponents: Population[Component3D]

given [T: Viewable3D]: Viewable3D[Population[T]] with
  extension (population: Population[T])
    def all3DComponents: Population[Component3D] =
      population.foldLeft(Population.empty[Component3D]){
        case (pop, (_, t)) =>
          pop ++ t.all3DComponents
      }

private given Conversion[SizeValue, DimensionBase] with
  def apply(sizeValue: SizeValue): DimensionBase =
    sizeValue.baseValue

private given Conversion[PositionValue, DimensionBase] with
  def apply(positionValue: PositionValue): DimensionBase =
    positionValue.baseValue

private given Conversion[Position, SpacePoint] with
  def apply(position: Position): SpacePoint =
    SpacePoint(
      position.x.value,
      position.y.value,
      -position.z.value
    )


given Viewable3D[Person] with
  extension (person: Person)
    override def all3DComponents: Population[Component3D] =
      Population(
        person.id ->
          Component3D(
            position = person.solid.mobile.position,
            basis = person.solid.basis,
            shape = person.shape
          )
      )

given (using Viewable3D[Person]): Viewable3D[Game] with
  extension (game: Game)
    override def all3DComponents: Population[Component3D] =
      game.world.all3DComponents ++ game.people.all3DComponents

given Viewable3D[World] with
  extension (world: World)
    override def all3DComponents: Population[Component3D] =
      val boundaries =
        Parallelepiped(
          width = world.area.width.value,
          height = world.area.height.value,
          depth = world.area.depth.value
        )
      val origin =
        Cross.Uniform((10))
      Population(
        world.id -> Component3D(
          SpacePoint(boundaries.width / Two, boundaries.height / Two, -boundaries.depth / Two),
          Basis.NormalDirect,
          boundaries + origin
        )
      )
