package com.ezoky.ezgames.covideo.system

import com.ezoky.ez3d.Screen.ScreenDimension
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.Dimension.DimensionBase
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*
import com.ezoky.ezgames.covideo.component.{HealthCondition, Position, Sprite}
import com.ezoky.ezgames.covideo.entity.{*, given}

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Display[T]:
  extension (entity: T) def display: IO[T]


given (using displaySystem: DisplaySystem): Display[ViewFrustum] with
  extension (viewFrustum: ViewFrustum)
    override def display: IO[ViewFrustum] =
      displaySystem.controlModel.flatMap {
        controlModel =>
          if !controlModel.updatedCamera then
            IO(viewFrustum)
          else
            val viewFrustumWithNear =
              if controlModel.camera.updatedNear then
                viewFrustum.withNear(controlModel.camera.near)
              else
                viewFrustum
            val controlModelWithNear =
              if controlModel.camera.updatedNear then
                controlModel.withCamera(controlModel.camera.withNear(viewFrustumWithNear.near).withFar(viewFrustumWithNear.far))
              else
                controlModel

            val viewFrustumWithFar =
              if controlModel.camera.updatedFar then
                viewFrustumWithNear.withFar(controlModelWithNear.camera.far)
              else
                viewFrustumWithNear
            val controlModelWithFar =
              if controlModel.camera.updatedFar then
                controlModelWithNear.withCamera(controlModelWithNear.camera.withFar(viewFrustumWithFar.far).withNear(viewFrustumWithFar.near))
              else
                controlModelWithNear
            displaySystem.updateControlModel(controlModelWithFar).map(_ => viewFrustumWithFar)

        //            val viewFrustumWithFar = viewFrustum.withFar(controlModel.camera.far).withNear(controlModel.camera.near)
        //            val controlModelWithFar = controlModel.withCamera(controlModel.camera.withFar(viewFrustumWithFar.far).withNear(viewFrustumWithFar.near))
        //            displaySystem.updateControlModel(controlModelWithFar).map(_ => viewFrustumWithFar)
      }

given (using DisplaySystem, Display[ViewFrustum]): Display[Camera] with
  extension (camera: Camera)
    override def display: IO[Camera] =
      for
        displayedViewFrustum <- camera.viewFrustum.display
      yield
        // cameras position must be consistent with View Frustum near value
        camera.withPosition(camera.position.withZ(displayedViewFrustum.near)).withViewFrustum(displayedViewFrustum)

given (using DisplaySystem, Display[Camera]): Display[Scene] with
  extension (scene: Scene)
    override def display: IO[Scene] =
      scene.camera.display.map(scene.withCamera(_))

given (using DisplaySystem, Display[Scene]): Display[World] with
  extension (world: World)
    override def display: IO[World] =
      world.scene.display.map(world.withScene(_))

given (using DisplaySystem, Display[World]): Display[Game] with
  extension (game: Game)
    override def display: IO[Game] =

      val displaySystem = summon[DisplaySystem]
      for
        displayedWorld <- game.world.display

        scene = displayedWorld.scene
        // We get all sprites
        sprites = game.people.map(_.sprite)
        // We get all 3D components
        components = game.all3DComponents

        displayedScene = scene.withSprites(sprites).withComponents(components)

        _ <- displaySystem.displayControl()
        _ <- displaySystem.displayScene(displayedScene)
      yield
        game.withWorld(
          displayedWorld.withScene(
            displayedScene
          )
        )


case class ControlModel(camera: CameraControl,
                        updatedCamera: Boolean = true):

  def withCamera(cameraControl: CameraControl): ControlModel =
    if !cameraControl.equalsState(camera) then
      copy(camera = cameraControl, updatedCamera = true)
    else
      this

  def acknowledgeUpdates(): ControlModel =
    copy(updatedCamera = false, camera = camera.ackowledgeUpdates())

  def equalsState(that: ControlModel): Boolean =
    this.camera == that.camera

case class CameraControl(near: DimensionBase,
                         far: DimensionBase,
                         maxNear: DimensionBase,
                         updatedNear: Boolean = true,
                         updatedFar: Boolean = true,
                         updatedMaxNear: Boolean = true):

  val depth = far - near
  val maxFar = maxNear + depth
  val minNear = 1
  val minFar = minNear + depth

  def withNear(near: DimensionBase): CameraControl =
    if near != this.near then
      copy(near = near, updatedNear = true)
    else
      this

  def withFar(far: DimensionBase): CameraControl =
    if far != this.far then
      copy(far = far, updatedFar = true)
    else
      this

  def withMaxNear(maxNear: DimensionBase): CameraControl =
    if maxNear != this.maxNear then
      copy(maxNear = maxNear, updatedMaxNear = true)
    else
      this

  def ackowledgeUpdates(): CameraControl =
    copy(updatedNear = false, updatedFar = false, updatedMaxNear = false)

  def equalsState(that: CameraControl): Boolean =
    this.near == that.near &&
      this.far == that.far &&
      this.maxNear == that.maxNear

trait DisplaySystem:

  // Generic
  def defaultScreenSceneDimension: ScreenDimension

  def controlModel: IO[ControlModel]

  def updateControlModel(model: ControlModel): IO[Unit]

  def displayScene(scene: Scene): IO[Unit]

  def displayControl(): IO[Unit]

  // Game specific
  def spriteByHealthCondition(healthCondition: HealthCondition): Sprite
