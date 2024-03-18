package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable, Coords}
import com.ezoky.ezgames.covideo.system.KeyboardCommand.KeyUp
import spire.*
import spire.implicits.*
import spire.math.*

trait UserControl[T]:
  extension (entityControl: T) def control(userCommand: UserCommand): T

type UserCommand = KeyboardCommand

trait KeyboardCommand

object KeyboardCommand:
  case object KeyUp extends KeyboardCommand

  case object KeyDown extends KeyboardCommand

  case object KeyLeft extends KeyboardCommand

  case object KeyRight extends KeyboardCommand

  case class KeyChar(char: Char) extends KeyboardCommand

sealed trait ItemControl:
  def ackowledgeUpdates(): ItemControl

  def equalsState(obj: ItemControl): Boolean



trait UserCommands[I: Identifiable, D: Dimension: Numeric]
    extends Coords[D]:

    import CoordsDimension.*

    case class UserControlConfig(cameraConfig: CameraControlConfig)

    case class CameraControlConfig(cameraUpStep: DimensionBase,
                                   cameraDownStep: DimensionBase,
                                   cameraLeftStep: DimensionBase,
                                   cameraRightStep: DimensionBase)

    given (using config: CameraControlConfig): UserControl[CameraControl] with
      extension (entityControl: CameraControl)
        def control(userCommand: UserCommand): CameraControl =
          userCommand match
            case KeyboardCommand.KeyUp =>
              entityControl.plusDy(-config.cameraUpStep)
            case KeyboardCommand.KeyDown =>
              entityControl.plusDy(config.cameraDownStep)
            case KeyboardCommand.KeyLeft =>
              entityControl.plusDx(-config.cameraLeftStep)
            case KeyboardCommand.KeyRight =>
              entityControl.plusDx(config.cameraRightStep)

//    given UserControl[ViewFrustumControl] with

    case class CameraControl(dx: DimensionBase = Zero,
                             dy: DimensionBase = Zero,
                             updatedDx: Boolean = true,
                             updatedDy: Boolean = true)
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

      def ackowledgeUpdates(): CameraControl =
        copy(dx = Zero, dy = Zero, updatedDx = false, updatedDy = false)

      def equalsState(obj: ItemControl): Boolean =
        obj match
          case that: CameraControl =>
            this.dx == that.dx &&
              this.dy == that.dy
          case _ =>
            false


    case class ViewFrustumControl(near: DimensionBase = Zero,
                                  far: DimensionBase = Zero,
                                  maxNear: DimensionBase = Zero,
                                  updatedNear: Boolean = true,
                                  updatedFar: Boolean = true,
                                  updatedMaxNear: Boolean = true)
      extends ItemControl:

      val depth = far - near
      val maxFar = maxNear + depth
      val minNear = One
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
