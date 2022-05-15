/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Coord.{XCoord, YCoord, ZCoord}
import com.ezoky.ezgames.covideo.component.Dimension.*

import scala.math.Ordering.Implicits.*

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Speed(xSpeed: XSpeed,
                 ySpeed: YSpeed,
                 zSpeed: ZSpeed):
  
  def move(position: Position,
           within: Area): Position =
    Position(
      xSpeed.move(position.x, within.width),
      ySpeed.move(position.y, within.height),
      zSpeed.move(position.z, within.depth),
    )

object Speed:

  val Zero: Speed =
    Speed(
      XSpeed.Zero,
      YSpeed.Zero,
      ZSpeed.Zero
    )

  def generated(xRange: SpeedRange,
                yRange: SpeedRange,
                zRange: SpeedRange): Generated[Speed] =
    Generated.map3(
      XSpeed.generatedWithin(xRange),
      YSpeed.generatedWithin(yRange),
      ZSpeed.generatedWithin(zRange)
    )(Speed(_, _, _))


trait SpeedCoord[C <: Coord](val value: SpeedValue):

  inline protected def newPosition[S <: Size[C]](position: PositionValue,
                                                 within: S): PositionValue =
    value.on(position)(using within.value, within.geometry)

  def move[S <: Size[C]](coord: C,
                         within: S): C


case class SpeedRange private(min: SpeedValue,
                              max: SpeedValue):
  def generatedSpeedValue: Generated[SpeedValue] =
    SpeedValue.generatedBetween(min, max)
    
  def truncate(speedValue: SpeedValue): SpeedValue =
    if (speedValue < min)
      min
    else if (speedValue > max)
      max
    else
      speedValue

object SpeedRange:

  val Null: SpeedRange =
    SpeedRange(SpeedValue.Zero, SpeedValue.Zero)

  def apply(min: SpeedValue,
            max: SpeedValue): SpeedRange =
    if (min <= max) {
      new SpeedRange(min, max)
    }
    else {
      new SpeedRange(max, min)
    }


case class XSpeed(override val value: SpeedValue)
  extends SpeedCoord[XCoord](value) :

  override def move[S <: Size[XCoord]](coord: XCoord,
                                       within: S): XCoord =
    XCoord(newPosition(coord.value, within))


object XSpeed:

  val Zero: XSpeed =
    XSpeed(SpeedValue.Zero)

  def generatedWithin(range: SpeedRange): Generated[XSpeed] =
    range.generatedSpeedValue.map(XSpeed(_))

case class YSpeed(override val value: SpeedValue)
  extends SpeedCoord[YCoord](value) :
  override def move[S <: Size[YCoord]](coord: YCoord,
                                       within: S): YCoord =
    YCoord(newPosition(coord.value, within))


object YSpeed:

  def Zero: YSpeed =
    YSpeed(SpeedValue.Zero)

  def generatedWithin(range: SpeedRange): Generated[YSpeed] =
    range.generatedSpeedValue.map(YSpeed(_))


case class ZSpeed(override val value: SpeedValue)
  extends SpeedCoord[ZCoord](value) :
  override def move[S <: Size[ZCoord]](coord: ZCoord,
                                       within: S): ZCoord =
    ZCoord(newPosition(coord.value, within))


object ZSpeed:

  def Zero: ZSpeed =
    ZSpeed(SpeedValue.Zero)

  def generatedWithin(range: SpeedRange): Generated[ZSpeed] =
    range.generatedSpeedValue.map(ZSpeed(_))
