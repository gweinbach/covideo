package com.ezoky.ezgames.covideo.system

import com.ezoky.ez3d.Screen.ScreenDimension
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.Dimension.DimensionBase
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*
import com.ezoky.ezgames.covideo.component.{HealthCondition, Position, Sprite}
import com.ezoky.ezgames.covideo.entity.{Component3D, Game, Scene, World}

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

given (using displaySystem: DisplaySystem): Display[ViewFrustum] with
  extension (entity: ViewFrustum)
    override def display: IO[ViewFrustum] =
      val controlModel = displaySystem.controlModel
      IO {
        entity.withNear(controlModel.camera.nearValue).withFar(controlModel.camera.farValue)
      }

given (using DisplaySystem, Display[ViewFrustum]): Display[Camera] with
  extension (entity: Camera)
    override def display: IO[Camera] =
      entity.viewFrustum.display.map(entity.withViewFrustum(_))

given (using DisplaySystem, Display[Camera]): Display[Scene] with
  extension (entity: Scene)
    override def display: IO[Scene] =
      entity.camera.display.map(entity.withCamera(_))

given (using DisplaySystem, Display[Scene]): Display[World] with
  extension (entity: World)
    override def display: IO[World] =
      entity.scene.display.map(entity.withScene(_))

given (using DisplaySystem, Display[World]): Display[Game] with
  extension (entity: Game)
    override def display: IO[Game] =

      val displaySystem = summon[DisplaySystem]
      for
        displayedWorld <- entity.world.display

        scene = displayedWorld.scene
        // We get all sprites 
        sprites = entity.people.map(_.sprite)
        // We get all 3D components 
        components =
          entity.people.map(
            person =>
              Component3D(
                position = person.solid.mobile.position,
                basis = person.solid.basis,
                shape = person.shape
              )
          )
        displayedScene = scene.withSprites(sprites).withComponents(components)

        _ <- displaySystem.initUI()
        _ <- displaySystem.drawScene(displayedScene)
      yield
        entity.withWorld(
          displayedWorld.withScene(
            displayedScene
          )
        )


case class ControlModel(camera: CameraControl)
case class CameraControl(nearValue: DimensionBase,
                         farValue: DimensionBase)


trait DisplaySystem:

  // Generic
  def defaultScreenSceneDimension: ScreenDimension

  def controlModel: ControlModel

  def drawScene(scene: Scene): IO[Unit]

  def initUI(): IO[Unit]

  // Game specific
  def spriteByHealthCondition(healthCondition: HealthCondition): Sprite
