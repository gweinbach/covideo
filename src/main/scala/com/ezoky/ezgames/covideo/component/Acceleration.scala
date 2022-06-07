package com.ezoky.ezgames.covideo.component

import Generate.*
import Dimension.{AccelerationValue, SpeedValue}

import scala.math.Ordering.Implicits.*

case class Acceleration(xAcceleration: XAcceleration,
                        yAcceleration: YAcceleration,
                        zAcceleration: ZAcceleration):

  def accelerate(speed: Speed,
                 within: SpeedRange): Speed =
    Speed(
      xAcceleration.accelerate(speed.xSpeed, within),
      yAcceleration.accelerate(speed.ySpeed, within),
      zAcceleration.accelerate(speed.zSpeed, within)
    )

  def truncate(within: AccelerationRange): Acceleration =
    copy(
      xAcceleration = xAcceleration.truncate(within),
      yAcceleration,
      zAcceleration
    )

trait Accelerating[T]:
  extension (accelerating: T) def acceleration: Acceleration
  

object Acceleration:

  val Zero: Acceleration =
    Acceleration(
      XAcceleration.Zero,
      YAcceleration.Zero,
      ZAcceleration.Zero
    )

  def generated(xRange: AccelerationRange,
                yRange: AccelerationRange,
                zRange: AccelerationRange): Generated[Acceleration] =
    Generated.map3(
      XAcceleration.generatedWithin(xRange),
      YAcceleration.generatedWithin(yRange),
      ZAcceleration.generatedWithin(zRange)
    )(Acceleration(_, _, _))


trait AccelerationCoord[C <: Coord, S <: SpeedCoord[C], A <: AccelerationCoord[C, S, A]](val value: AccelerationValue):

  inline protected def newSpeed(speed: SpeedValue): SpeedValue =
    value(speed)

  def accelerate(speed: S,
                 within: SpeedRange): S

  def truncate(within: AccelerationRange): A


/**
 * inclusive on both bounds
 */
case class AccelerationRange private(min: AccelerationValue,
                                     max: AccelerationValue):
  def generatedAccelerationValue: Generated[AccelerationValue] =
    AccelerationValue.generatedBetween(min, max)

  def truncate(accelerationValue: AccelerationValue): AccelerationValue =
    if (accelerationValue < min)
      min
    else if (accelerationValue > max)
      max
    else
      accelerationValue


object AccelerationRange:

  val Null: AccelerationRange =
    AccelerationRange(AccelerationValue.Zero, AccelerationValue.Zero)

  def apply(min: AccelerationValue,
            max: AccelerationValue): AccelerationRange =
    if (min <= max)
      new AccelerationRange(min, max)

    else
      new AccelerationRange(max, min)


case class XAcceleration(override val value: AccelerationValue)
  extends AccelerationCoord[XCoord, XSpeed, XAcceleration](value) :

  override def accelerate(speed: XSpeed,
                          within: SpeedRange): XSpeed =
    XSpeed(within.truncate(newSpeed(speed.value)))

  override def truncate(within: AccelerationRange): XAcceleration =
    copy(value = within.truncate(value))

object XAcceleration:

  val Zero: XAcceleration =
    XAcceleration(AccelerationValue.Zero)

  def generatedWithin(range: AccelerationRange): Generated[XAcceleration] =
    range.generatedAccelerationValue.map(XAcceleration(_))

case class YAcceleration(override val value: AccelerationValue)
  extends AccelerationCoord[YCoord, YSpeed, YAcceleration](value) :

  override def accelerate(speed: YSpeed,
                          within: SpeedRange): YSpeed =
    YSpeed(within.truncate(newSpeed(speed.value)))

  override def truncate(within: AccelerationRange): YAcceleration =
    copy(value = within.truncate(value))

object YAcceleration:

  def Zero: YAcceleration =
    YAcceleration(AccelerationValue.Zero)

  def generatedWithin(range: AccelerationRange): Generated[YAcceleration] =
    range.generatedAccelerationValue.map(YAcceleration(_))


case class ZAcceleration(override val value: AccelerationValue)
  extends AccelerationCoord[ZCoord, ZSpeed, ZAcceleration](value) :

  override def accelerate(speed: ZSpeed,
                          within: SpeedRange): ZSpeed =
    ZSpeed(within.truncate(newSpeed(speed.value)))

  override def truncate(within: AccelerationRange): ZAcceleration =
    copy(value = within.truncate(value))

object ZAcceleration:

  def Zero: ZAcceleration =
    ZAcceleration(AccelerationValue.Zero)

  def generatedWithin(range: AccelerationRange): Generated[ZAcceleration] =
    range.generatedAccelerationValue.map(ZAcceleration(_))
