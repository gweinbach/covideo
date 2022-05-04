package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Coord.{XCoord, YCoord, ZCoord}
import com.ezoky.ezgames.covideo.component.Dimension.{AccelerationValue, SpeedValue}

import scala.math.Ordering.Implicits.*

case class Acceleration(xAcceleration: XAcceleration,
                        yAcceleration: YAcceleration,
                        zAcceleration: ZAcceleration):

  def accelerate(speed: Speed): Speed =
    Speed(
      xAcceleration.accelerate(speed.xSpeed),
      yAcceleration.accelerate(speed.ySpeed),
      zAcceleration.accelerate(speed.zSpeed)
    )

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


trait AccelerationCoord[C <: Coord, S <: SpeedCoord[C]](val value: AccelerationValue):

  inline protected def newSpeed(speed: SpeedValue): SpeedValue =
    value(speed)

  def accelerate(speed: S): S


case class AccelerationRange private(min: AccelerationValue,
                                     max: AccelerationValue):
  def generatedAccelerationValue: Generated[AccelerationValue] =
    AccelerationValue.generatedBetween(min, max)

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
  extends AccelerationCoord[XCoord, XSpeed](value) :

  override def accelerate(speed: XSpeed): XSpeed =
    XSpeed(newSpeed(speed.value))


object XAcceleration:

  val Zero: XAcceleration =
    XAcceleration(AccelerationValue.Zero)

  def generatedWithin(range: AccelerationRange): Generated[XAcceleration] =
    range.generatedAccelerationValue.map(XAcceleration(_))

case class YAcceleration(override val value: AccelerationValue)
  extends AccelerationCoord[YCoord, YSpeed](value) :

  override def accelerate(speed: YSpeed): YSpeed =
    YSpeed(newSpeed(speed.value))


object YAcceleration:

  def Zero: YAcceleration =
    YAcceleration(AccelerationValue.Zero)

  def generatedWithin(range: AccelerationRange): Generated[YAcceleration] =
    range.generatedAccelerationValue.map(YAcceleration(_))


case class ZAcceleration(override val value: AccelerationValue)
  extends AccelerationCoord[ZCoord, ZSpeed](value) :

  override def accelerate(speed: ZSpeed): ZSpeed =
    ZSpeed(newSpeed(speed.value))


object ZAcceleration:

  def Zero: ZAcceleration =
    ZAcceleration(AccelerationValue.Zero)

  def generatedWithin(range: AccelerationRange): Generated[ZAcceleration] =
    range.generatedAccelerationValue.map(ZAcceleration(_))
