package com.ezoky.ezgames.covideo.system

import com.ezoky.ez3d.Screen.ScreenDimension
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.SpacePoint
import com.ezoky.ezgames.covideo.component.{HealthCondition, Position, Sprite}
import com.ezoky.ezgames.covideo.entity.{Game, Scene, Component3D}

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Display[T]:
  extension (entity: T) def display: IO[T]


// TODO à revoir
private given Conversion[Position, SpacePoint] with
  def apply(position: Position): SpacePoint =
    SpacePoint(
      position.x.value.doubleValue,
      position.y.value.doubleValue,
      position.z.value.doubleValue
    )

given (using DisplaySystem): Display[Game] with
  extension (entity: Game)
    override def display: IO[Game] =
      val scene = entity.world.scene

      val sprites = entity.people.map(_.sprite)
      val sceneWithSprites = scene.withSprites(sprites)

      val components =
        entity.people.map(
          person =>
            Component3D(
              position = person.solid.mobile.position,
              basis = person.solid.basis,
              shape = person.shape
            )
        )
      val sceneWithComponents = sceneWithSprites.withComponents(components)

      for
        _ <- summon[DisplaySystem].drawScene(sceneWithComponents)
      yield
        entity.copy(
          world = entity.world.copy(
            scene = sceneWithComponents
          )
        )

trait DisplaySystem:

  // Generic
  def defaultScreenSceneDimension: ScreenDimension

  def drawScene(scene: Scene): IO[Unit]

  // Game specific
  def spriteByHealthCondition(healthCondition: HealthCondition): Sprite
