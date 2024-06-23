package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Coords, Dimension, Identifiable}
import spire.*
import spire.implicits.*
import spire.math.*

trait UserControl[T]:
  extension (entityControl: T) def control(userCommand: UserCommand): T

// We might add other Commands than Keyboard Commands (Mouse Commands ?)
type UserCommand = KeyboardCommand | LifeCycleEvent

trait KeyboardCommand

object KeyboardCommand:
  case object KeyUp extends KeyboardCommand

  case object KeyDown extends KeyboardCommand

  case object KeyLeft extends KeyboardCommand

  case object KeyRight extends KeyboardCommand

  case class KeyChar(char: Char) extends KeyboardCommand


trait LifeCycleEvent

object LifeCycleEvent:
  case object DisplayControls extends LifeCycleEvent

  case object DisplayScene extends LifeCycleEvent

  case object DisposeControls extends LifeCycleEvent

  case object DisposeScene extends LifeCycleEvent


sealed trait ItemControl:

  def isUpdated: Boolean

  def ackowledgeUpdates(): ItemControl

  def equalsState(obj: ItemControl): Boolean

sealed trait ControlledItem:
  type ItemControlType <: ItemControl

private class Controller(val controlledItem: ControlledItem,
                         val itemControl: controlledItem.ItemControlType,
                         val updatedControl: Boolean = false):

  def updateItem(newItemControl: controlledItem.ItemControlType): Controller =
    new Controller(controlledItem, newItemControl, updatedControl = true)

  def ackowledgeUpdates(): Controller =
    new Controller(controlledItem, itemControl.ackowledgeUpdates().asInstanceOf[controlledItem.ItemControlType], updatedControl = false)


case class ControlModel(private val controllers: Map[ControlledItem, Controller]):

  def getControl(controlledItem: ControlledItem): controlledItem.ItemControlType =
    controllers.apply(controlledItem).itemControl.asInstanceOf[controlledItem.ItemControlType]

  def isUpdated(controlledItem: ControlledItem): Boolean =
    controllers.apply(controlledItem).updatedControl

  def withControl(controlledItem: ControlledItem,
                  itemControl: controlledItem.ItemControlType): ControlModel =
    controllers.get(controlledItem) match
      case Some(controller) if !controller.itemControl.equalsState(itemControl) =>
        copy(controllers = controllers + (controlledItem -> controller.updateItem(itemControl.asInstanceOf[controller.controlledItem.ItemControlType])))
      case None =>
        copy(controllers = controllers + (controlledItem -> Controller(controlledItem, itemControl)))
      case _ =>
        this

  def updateControl(controlledItem: ControlledItem,
                    update: controlledItem.ItemControlType => controlledItem.ItemControlType): ControlModel =
    withControl(controlledItem, update(getControl(controlledItem)))

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

trait UserCommands[I: Identifiable, D: Dimension : Numeric]
  extends Coords[D]:

  import CoordsDimension.*

  object ControlledItem:
    case object Game extends ControlledItem:
      override type ItemControlType = GameControl

    case object ViewFrustum extends ControlledItem:
      override type ItemControlType = ViewFrustumControl

    case object Camera extends ControlledItem:
      override type ItemControlType = CameraControl

  case class UserControlConfig(gameConfig: GameControlConfig,
                               cameraConfig: CameraControlConfig,
                               viewFrustumConfig: ViewFrustumControlConfig)

  case class GameControlConfig(exitChar: Char)

  case class CameraControlConfig(cameraUpStep: DimensionBase,
                                 cameraDownStep: DimensionBase,
                                 cameraLeftStep: DimensionBase,
                                 cameraRightStep: DimensionBase)

  case class ViewFrustumControlConfig(minNear: DimensionBase,
                                      maxNear: DimensionBase,
                                      minFar: DimensionBase,
                                      maxFar: DimensionBase,
                                      initialNear: DimensionBase,
                                      initialFar: DimensionBase)

  given (using config: UserControlConfig,
         _viewFrustumUserControl: UserControl[ViewFrustumControl],
         _cameraUserControl: UserControl[CameraControl],
         _gameUserControl: UserControl[GameControl]): UserControl[ControlModel] with
    extension (entityControl: ControlModel)
      def control(userCommand: UserCommand): ControlModel =
        given GameControlConfig = config.gameConfig

        given CameraControlConfig = config.cameraConfig

        given ViewFrustumControlConfig = config.viewFrustumConfig

        val viewFrustumControl = entityControl.getControl(ControlledItem.ViewFrustum).control(userCommand)
        val cameraControl = entityControl.getControl(ControlledItem.Camera).control(userCommand)
        val gameControl = entityControl.getControl(ControlledItem.Game).control(userCommand)
        ControlModel(
          gameControl,
          viewFrustumControl,
          cameraControl
        )


  /**
   * This maps User Commands on Camera Control
   *
   * Game specific
   * TODO: extract this to a specific package
   */
  given (using config: CameraControlConfig): UserControl[CameraControl] with
    extension (entityControl: CameraControl)
      def control(userCommand: UserCommand): CameraControl =
        userCommand match
          case KeyboardCommand.KeyUp =>
            entityControl.plusDy(config.cameraUpStep)
          case KeyboardCommand.KeyDown =>
            entityControl.plusDy(-config.cameraDownStep)
          case KeyboardCommand.KeyLeft =>
            entityControl.plusDx(-config.cameraLeftStep)
          case KeyboardCommand.KeyRight =>
            entityControl.plusDx(config.cameraRightStep)
          case _ =>
            entityControl

  /**
   * This maps User Commands on Game Control
   *
   * Game specific
   * TODO: extract this to a specific package
   */
  given (using config: GameControlConfig): UserControl[GameControl] with
    extension (entityControl: GameControl)
      override def control(userCommand: UserCommand): GameControl =
        userCommand match
          case KeyboardCommand.KeyChar(config.exitChar) =>
            entityControl.exit()
          case _ =>
            entityControl

  /**
   * This maps User Commands on ViewFrustum Control
   *
   * Game specific
   * TODO: extract this to a specific package
   */
  given (using config: ViewFrustumControlConfig): UserControl[ViewFrustumControl] with
    extension (entityControl: ViewFrustumControl)
      override def control(userCommand: UserCommand): ViewFrustumControl =
        userCommand match
          case LifeCycleEvent.DisplayControls =>
            entityControl
              .withMinNear(config.minNear)
              .withMaxNear(config.maxNear)
              .withMinFar(config.minFar)
              .withMaxFar(config.maxFar)
              .withNear(config.initialNear)
              .withFar(config.initialFar)
          case _ =>
            entityControl


  case class GameControl(populationSize: Int = 0,
                         doExit: Boolean = false,
                         updatedPopulationSize: Boolean = false,
                         updatedExited: Boolean = false)
    extends ItemControl:

    def withPopulationSize(populationSize: Int): GameControl =
      copy(populationSize = populationSize, updatedPopulationSize = true)

    def exit(): GameControl =
      copy(doExit = true, updatedExited = true)

    override def isUpdated: Boolean =
      updatedPopulationSize || updatedExited

    override def ackowledgeUpdates(): ItemControl =
      copy(updatedPopulationSize = false, updatedExited = false)

    override def equalsState(obj: ItemControl): Boolean =
      obj match
        case that: GameControl =>
          (this.populationSize == that.populationSize) &&
            (this.doExit == that.doExit)
        case _ =>
          false


  case class CameraControl(dx: DimensionBase = Zero,
                           dy: DimensionBase = Zero,
                           updatedDx: Boolean = false,
                           updatedDy: Boolean = false)
    extends ItemControl:

    def plusDx(addedDx: DimensionBase): CameraControl =
      if dx + addedDx != Zero then
        copy(dx = dx + addedDx, updatedDx = true)
      else
        copy(dx = Zero, updatedDx = false)

    def plusDy(addedDy: DimensionBase): CameraControl =
      if dy + addedDy != Zero then
        copy(dy = dy + addedDy, updatedDy = true)
      else
        copy(dy = Zero, updatedDy = false)

    def hasMoved: Boolean =
      dx != Zero || dy != Zero

    override def isUpdated: Boolean =
      updatedDx || updatedDy

    override def ackowledgeUpdates(): CameraControl =
      copy(dx = Zero, dy = Zero, updatedDx = false, updatedDy = false)

    override def equalsState(obj: ItemControl): Boolean =
      obj match
        case that: CameraControl =>
          this.dx == that.dx &&
            this.dy == that.dy
        case _ =>
          false


  case class ViewFrustumControl(near: DimensionBase = Zero,
                                far: DimensionBase = Zero,
                                minNear: DimensionBase = Zero,
                                maxNear: DimensionBase = Zero,
                                minFar: DimensionBase = Zero,
                                maxFar: DimensionBase = Zero,
                                lockDepth: Boolean = true,
                                updatedNear: Boolean = false,
                                updatedFar: Boolean = false,
                                updatedMinNear: Boolean = false,
                                updatedMaxNear: Boolean = false,
                                updatedMinFar: Boolean = false,
                                updatedMaxFar: Boolean = false)
    extends ItemControl:

    val depth = far - near

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

    def withLockDepth(lockDepth: Boolean): ViewFrustumControl =
      if lockDepth != this.lockDepth then
        copy(lockDepth = lockDepth)
      else
        this

    def withMinNear(minNear: DimensionBase): ViewFrustumControl =
      if minNear != this.minNear then
        copy(minNear = minNear, updatedMinNear = true)
      else
        this

    def withMinFar(minFar: DimensionBase): ViewFrustumControl =
      if minFar != this.minFar then
        copy(minFar = minFar, updatedMinFar = true)
      else
        this

    def withMaxNear(maxNear: DimensionBase): ViewFrustumControl =
      if maxNear != this.maxNear then
        copy(maxNear = maxNear, updatedMaxNear = true)
      else
        this

    def withMaxFar(maxFar: DimensionBase): ViewFrustumControl =
      if maxFar != this.maxFar then
        copy(maxFar = maxFar, updatedMaxFar = true)
      else
        this

    override def isUpdated: Boolean =
      updatedNear || updatedFar ||
        updatedMinNear || updatedMinFar ||
        updatedMaxNear || updatedMaxFar

    override def ackowledgeUpdates(): ViewFrustumControl =
      copy(
        updatedNear = false,
        updatedFar = false,
        updatedMinNear = false,
        updatedMaxNear = false,
        updatedMinFar = false,
        updatedMaxFar = false
      )

    override def equalsState(obj: ItemControl): Boolean =
      obj match
        case that: ViewFrustumControl =>
          this.near == that.near &&
            this.far == that.far &&
            this.maxNear == that.maxNear &&
            this.lockDepth == that.lockDepth
        case _ =>
          false


  object ControlModel:
    def apply(game: GameControl,
              viewFrustum: ViewFrustumControl,
              camera: CameraControl): ControlModel =
      new ControlModel(
        Map(
          ControlledItem.Game -> Controller(ControlledItem.Game, game),
          ControlledItem.ViewFrustum -> Controller(ControlledItem.ViewFrustum, viewFrustum),
          ControlledItem.Camera -> Controller(ControlledItem.Camera, camera)
        )
      )