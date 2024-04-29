package com.ezoky.ezgames.covideo.system

import com.ezoky.ez3d.Screen.ScreenDimension
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.{Dimension, HealthCondition, Identifiable}
import com.ezoky.ezgames.covideo.entity.{Games, Scenes, Viewables, Worlds}
import spire.*
import spire.math.*

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Display[T]:
  extension (entity: T)
    /**
     * Should update display according to:
     * <ul>
     * <li>displayed content</li>
     * <li>control model values (that might influence display)</li>
     * </ul>
     *
     * Control model itself might also be updated as a consequence of evolution of displayed content.
     *
     * @return IO[T] as displaying is a side effect
     */
    def display: IO[T]

trait Displays[I: Identifiable, D: Dimension : Numeric]
  extends Games[I, D]
    with Worlds[I, D]
    with Scenes[I, D]
    with Viewables[I, D]
    with UserCommands[I, D]:

  import CoordsDimension.Ez3D.*
  import CoordsDimension.given

  abstract class DisplaySystem(userControlConfig: UserControlConfig):

    // Generic
    def defaultScreenSceneDimension: ScreenDimension

    def displayControls(): IO[Unit]

    def popControlModel(item: ControlledItem): IO[ControlModel]

    def updateControlModel(model: ControlModel): IO[Unit]

    def displayScene(scene: Scene): IO[Unit]

    def dispose(doDispose: Boolean): IO[Unit]

    /**
     * Game specific
     * TODO: extract this to a specific package
     */
    def spriteByHealthCondition(healthCondition: HealthCondition): Sprite


  given (using displaySystem: DisplaySystem): Display[ViewFrustum] with
    extension (viewFrustum: ViewFrustum)
      override def display: IO[ViewFrustum] =
        displaySystem.popControlModel(ControlledItem.ViewFrustum).flatMap {
          controlModel =>
            if !controlModel.isUpdated(ControlledItem.ViewFrustum) then
              IO(viewFrustum)
            else
              val viewFrustumWithNear =
                if controlModel.getControl(ControlledItem.ViewFrustum).updatedNear then
                  viewFrustum.withNear(controlModel.getControl(ControlledItem.ViewFrustum).near)
                else
                  viewFrustum
              val controlModelWithNear =
                if controlModel.getControl(ControlledItem.ViewFrustum).updatedNear then
                  controlModel.updateControl(ControlledItem.ViewFrustum, _.withNear(viewFrustumWithNear.near).withFar(viewFrustumWithNear.far))
                else
                  controlModel

              val viewFrustumWithFar =
                if controlModel.getControl(ControlledItem.ViewFrustum).updatedFar then
                  viewFrustumWithNear.withFar(controlModelWithNear.getControl(ControlledItem.ViewFrustum).far)
                else
                  viewFrustumWithNear
              val controlModelWithFar =
                if controlModel.getControl(ControlledItem.ViewFrustum).updatedFar then
                  controlModelWithNear.updateControl(ControlledItem.ViewFrustum, _.withFar(viewFrustumWithFar.far).withNear(viewFrustumWithFar.near))
                else
                  controlModelWithNear

              displaySystem.updateControlModel(controlModelWithFar).map(_ => viewFrustumWithFar)
        }

  given (using DisplaySystem, Display[ViewFrustum]): Display[Camera] with
    extension (camera: Camera)
      override def display: IO[Camera] =
        for
          controlModel <- summon[DisplaySystem].popControlModel(ControlledItem.Camera)
          cameraControl = controlModel.getControl(ControlledItem.Camera)
          movedCamera = camera.move(cameraControl.dx, cameraControl.dy)
          displayedViewFrustum <- movedCamera.viewFrustum.display
        yield
          // cameras position must be consistent with View Frustum near value
          //        movedCamera.withPosition(movedCamera.position.withZ(displayedViewFrustum.near)).withViewFrustum(displayedViewFrustum)
          movedCamera.withViewFrustum(displayedViewFrustum)

  given (using DisplaySystem, Display[Camera]): Display[Scene] with
    extension (scene: Scene)
      override def display: IO[Scene] =
        scene.camera.display.map(scene.withCamera(_))

  given (using DisplaySystem, Display[Scene]): Display[World] with
    extension (world: World)
      override def display: IO[World] =

        val displaySystem = summon[DisplaySystem]
        for
          displayedScene <- world.scene.display

          // side effects
          _ <- displaySystem.displayScene(displayedScene)
          _ <- displaySystem.displayControls()
        yield
          world.withScene(displayedScene)

  given (using DisplaySystem, Display[World]): Display[Game] with
    extension (game: Game)
      override def display: IO[Game] =

        val displaySystem = summon[DisplaySystem]
        for
          controlModel <- displaySystem.popControlModel(ControlledItem.Game)
          gameControl = controlModel.getControl(ControlledItem.Game)
          controlledGame = if gameControl.doExit then
            game.terminate()
          else
            game

          displayedWorld <- controlledGame.world.display

          // side effects
          _ <- displaySystem.dispose(gameControl.doExit) 

        yield
          controlledGame.withWorld(
            displayedWorld
          )



