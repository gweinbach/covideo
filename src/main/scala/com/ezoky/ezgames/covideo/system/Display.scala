package com.ezoky.ezgames.covideo.system

import com.ezoky.ez3d.Cameras
import com.ezoky.ez3d.Screen.ScreenDimension
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.{Dimension, HealthCondition, Identifiable}
import com.ezoky.ezgames.covideo.entity.{Games, Scenes, Viewables, Worlds}
import spire.*
import spire.implicits.*
import spire.math.*

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Display[T]:
  extension (entity: T) def display: IO[T]

trait Displays[I: Identifiable, D: Dimension : Numeric]
  extends Games[I, D]
    with Worlds[I, D]
    with Scenes[I, D]
    with Viewables[I, D]
    with UserCommands[I, D]:

  import CoordsDimension.Ez3D.*
  import CoordsDimension.{*, given}

  abstract class DisplaySystem(userControlConfig: UserControlConfig):

    // Generic
    def defaultScreenSceneDimension: ScreenDimension

    def popControlModel(item: ControlledItem): IO[ControlModel]

    def updateControlModel(model: ControlModel): IO[Unit]

    def displayScene(scene: Scene): IO[Unit]

    def displayControl(): IO[Unit]

    // Game specific
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
                if controlModel.control(ControlledItem.ViewFrustum).updatedNear then
                  viewFrustum.withNear(controlModel.control(ControlledItem.ViewFrustum).near)
                else
                  viewFrustum
              val controlModelWithNear =
                if controlModel.control(ControlledItem.ViewFrustum).updatedNear then
                  controlModel.updateControl(ControlledItem.ViewFrustum, _.withNear(viewFrustumWithNear.near).withFar(viewFrustumWithNear.far))
                else
                  controlModel

              val viewFrustumWithFar =
                if controlModel.control(ControlledItem.ViewFrustum).updatedFar then
                  viewFrustumWithNear.withFar(controlModelWithNear.control(ControlledItem.ViewFrustum).far)
                else
                  viewFrustumWithNear
              val controlModelWithFar =
                if controlModel.control(ControlledItem.ViewFrustum).updatedFar then
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
          cameraControl = controlModel.control(ControlledItem.Camera)
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
        world.scene.display.map(world.withScene(_))

  given (using DisplaySystem, Display[World]): Display[Game] with
    extension (game: Game)
      override def display: IO[Game] =

        val displaySystem = summon[DisplaySystem]
        for
          displayedWorld <- game.world.display

          scene = displayedWorld.scene

          // We get all sprites
          sprites: Population[Sprite] = game.allViewables

          // We get all 3D components
          components: Population[Component3D] = game.allViewables

          displayedScene = scene.withSprites(sprites).withComponents(components)

          _ <- displaySystem.displayControl()
          _ <- displaySystem.displayScene(displayedScene)
        yield
          game.withWorld(
            displayedWorld.withScene(
              displayedScene
            )
          )

  sealed trait ControlledItem:
    type ItemControlType <: ItemControl

  object ControlledItem:
    case object ViewFrustum extends ControlledItem:
      override type ItemControlType = ViewFrustumControl

    case object Camera extends ControlledItem:
      override type ItemControlType = CameraControl


  private class Controller(val controlledItem: ControlledItem,
                           val itemControl: controlledItem.ItemControlType,
                           val updatedControl: Boolean = true):

    def updateItem(newItemControl: controlledItem.ItemControlType): Controller =
      new Controller(controlledItem, newItemControl, updatedControl = true)

    def ackowledgeUpdates(): Controller =
      new Controller(controlledItem, itemControl.ackowledgeUpdates().asInstanceOf[controlledItem.ItemControlType], updatedControl = false)


  case class ControlModel private(private val controllers: Map[ControlledItem, Controller]):

    def control(controlledItem: ControlledItem): controlledItem.ItemControlType =
      controllers.apply(controlledItem).itemControl.asInstanceOf[controlledItem.ItemControlType]

    def isUpdated(controlledItem: ControlledItem): Boolean =
      controllers.apply(controlledItem).updatedControl

    def withControl(controlledItem: ControlledItem,
                    itemControl: controlledItem.ItemControlType): ControlModel =
      val optController = controllers.get(controlledItem)
      optController match
        case Some(controller) if !controller.itemControl.equalsState(itemControl) =>
          copy(controllers = controllers + (controlledItem -> controller.updateItem(itemControl.asInstanceOf[controller.controlledItem.ItemControlType])))
        case None =>
          copy(controllers = controllers + (controlledItem -> Controller(controlledItem, itemControl)))
        case _ =>
          this

    def updateControl(controlledItem: ControlledItem,
                      update: controlledItem.ItemControlType => controlledItem.ItemControlType): ControlModel =
      withControl(controlledItem, update(control(controlledItem)))

    def acknowledgeUpdates(controlledItem: ControlledItem): ControlModel =
      val controller = controllers.apply(controlledItem)
      copy(controllers = controllers + (controlledItem -> controller.ackowledgeUpdates()))

    def equalsState(that: ControlModel): Boolean =
      controllers.foldLeft(true) {
        case (eq, (controlledItem, controller)) =>
          that.controllers.get(controlledItem).fold(false)(
            thatController =>
              eq && controller.itemControl.equalsState(thatController.itemControl)
          )
      }

  object ControlModel:
    def apply(viewFrustum: ViewFrustumControl,
              camera: CameraControl): ControlModel =
      new ControlModel(
        Map(
          ControlledItem.ViewFrustum -> Controller(ControlledItem.ViewFrustum, viewFrustum),
          ControlledItem.Camera -> Controller(ControlledItem.Camera, camera)
        )
      )


