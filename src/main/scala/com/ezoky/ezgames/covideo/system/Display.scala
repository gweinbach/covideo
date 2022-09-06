package com.ezoky.ezgames.covideo.system

import com.ezoky.ez3d.Screen.ScreenDimension
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.Dimension.DimensionBase
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*
import com.ezoky.ezgames.covideo.component.{HealthCondition, Position, Sprite}
import com.ezoky.ezgames.covideo.entity.People.Population
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
                controlModel.withControl(ControlledItem.ViewFrustum, controlModel.control(ControlledItem.ViewFrustum).withNear(viewFrustumWithNear.near).withFar(viewFrustumWithNear.far))
              else
                controlModel

            val viewFrustumWithFar =
              if controlModel.control(ControlledItem.ViewFrustum).updatedFar then
                viewFrustumWithNear.withFar(controlModelWithNear.control(ControlledItem.ViewFrustum).far)
              else
                viewFrustumWithNear
            val controlModelWithFar =
              if controlModel.control(ControlledItem.ViewFrustum).updatedFar then
                controlModelWithNear.withControl(ControlledItem.ViewFrustum, controlModelWithNear.control(ControlledItem.ViewFrustum).withFar(viewFrustumWithFar.far).withNear(viewFrustumWithFar.near))
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

sealed trait ItemControl:
  def ackowledgeUpdates():ItemControl
  def equalsState(obj: ItemControl): Boolean


class Controller(val controlledItem: ControlledItem,
                 val itemControl: controlledItem.ItemControlType,
                 val updatedControl: Boolean = true):

  def updateItem(newItemControl: controlledItem.ItemControlType): Controller =
    new Controller(controlledItem, newItemControl, updatedControl = true)

  def ackowledgeUpdates(): Controller =
    new Controller(controlledItem, itemControl.ackowledgeUpdates().asInstanceOf[controlledItem.ItemControlType], updatedControl = false)


case class ControlModel private(controllers: Map[ControlledItem, Controller]):

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

  def acknowledgeUpdates(controlledItem: ControlledItem): ControlModel =
    val controller = controllers.apply(controlledItem)
    copy(controllers = controllers + (controlledItem -> controller.ackowledgeUpdates()))

//  def acknowledgeUpdates(): ControlModel =
//    val ackControllers =
//      controllers.foldLeft(Map.empty[ControlledItem,Controller]) {
//        case (map, (controlledItem, controller)) =>
//          map + (controlledItem -> controller.ackowledgeUpdates())
//      }
//    copy(controllers = ackControllers)

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

//case class ControlModel1(viewFrustum: ViewFrustumControl,
//                        camera: CameraControl,
//                        updatedViewFrustum: Boolean = true,
//                        updatedCamera: Boolean = true):
//
//  def withViewFrustum(viewFrustumControl: ViewFrustumControl): ControlModel1 =
//    if !viewFrustumControl.equalsState(viewFrustum) then
//      copy(viewFrustum = viewFrustumControl, updatedViewFrustum = true)
//    else
//      this
//
//  def withCamera(cameraControl: CameraControl): ControlModel1 =
//    if !cameraControl.equalsState(camera) then
//      copy(camera = cameraControl, updatedCamera = true)
//    else
//      this
//
//  def acknowledgeUpdates(): ControlModel1 =
//    copy(updatedViewFrustum = false, viewFrustum = viewFrustum.ackowledgeUpdates())
//
//  def equalsState(that: ControlModel1): Boolean =
//    this.viewFrustum == that.viewFrustum && this.camera == that.camera
    
    
case class CameraControl(dx: DimensionBase = 0,
                         dy: DimensionBase = 0,
                         updatedDx: Boolean = true,
                         updatedDy: Boolean = true)
  extends ItemControl:

  def plusDx(addedDx: DimensionBase): CameraControl =
    if dx + addedDx != 0 then
      copy(dx = dx + addedDx, updatedDx = true)
    else
      copy(dx = 0, updatedDx = false)

  def plusDy(addedDy: DimensionBase): CameraControl =
    if dy + addedDy != 0 then
      copy(dy = dy + addedDy, updatedDy = true)
    else
      copy(dy = 0, updatedDy = false)

  def ackowledgeUpdates(): CameraControl =
    copy(dx = 0, dy = 0, updatedDx = false, updatedDy = false)

  def equalsState(obj: ItemControl): Boolean =
    obj match
      case that: CameraControl =>
        this.dx == that.dx &&
          this.dy == that.dy
      case _ =>
        false


case class ViewFrustumControl(near: DimensionBase,
                              far: DimensionBase,
                              maxNear: DimensionBase,
                              updatedNear: Boolean = true,
                              updatedFar: Boolean = true,
                              updatedMaxNear: Boolean = true)
  extends ItemControl:

  val depth = far - near
  val maxFar = maxNear + depth
  val minNear = 1
  val minFar = minNear + depth

  def withNear(near: DimensionBase): ViewFrustumControl =
    if near != this.near then
      copy(near = near, updatedNear = true)
    else
      this

  def withFar(far: DimensionBase): ViewFrustumControl =
    if far != this.far then
      copy(far = far, updatedFar = true)
    else
      this

  def withMaxNear(maxNear: DimensionBase): ViewFrustumControl =
    if maxNear != this.maxNear then
      copy(maxNear = maxNear, updatedMaxNear = true)
    else
      this

  def ackowledgeUpdates(): ViewFrustumControl =
    copy(updatedNear = false, updatedFar = false, updatedMaxNear = false)

  def equalsState(obj: ItemControl): Boolean =
    obj match
      case that: ViewFrustumControl =>
        this.near == that.near &&
          this.far == that.far &&
          this.maxNear == that.maxNear
      case _ =>
        false

trait DisplaySystem:

  // Generic
  def defaultScreenSceneDimension: ScreenDimension

  def popControlModel(item: ControlledItem): IO[ControlModel]

  def updateControlModel(model: ControlModel): IO[Unit]

  def displayScene(scene: Scene): IO[Unit]

  def displayControl(): IO[Unit]

  // Game specific
  def spriteByHealthCondition(healthCondition: HealthCondition): Sprite
