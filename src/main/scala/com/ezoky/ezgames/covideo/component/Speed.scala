/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Coord.{XCoord, YCoord, ZCoord}
import com.ezoky.ezgames.covideo.component.Dimension.*
import Ordering.Implicits.*

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Speed[C <: Coord](val value: SpeedValue) {

  inline protected def newPosition[S <: Size[C]](position: PositionValue,
                                                 within: S): PositionValue =
    value.on(position)(using within.value, within.geometry)

  def move[S <: Size[C]](coord: C,
                         within: S): C
}

case class SpeedRange private (min: SpeedValue,
                               max: SpeedValue)

object SpeedRange {

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
}

case class XSpeed(override val value: SpeedValue)
  extends Speed[XCoord](value) {

  override def move[S <: Size[XCoord]](coord: XCoord,
                                       within: S): XCoord =
    XCoord(newPosition(coord.value, within))
}

object XSpeed {

  val Zero: XSpeed =
    XSpeed(SpeedValue.Zero)

  def RandomWithin(range: SpeedRange): XSpeed =
    XSpeed(
      SpeedValue.RandomBetween(range.min, range.max)
    )
}

case class YSpeed(override val value: SpeedValue)
  extends Speed[YCoord](value) {
  override def move[S <: Size[YCoord]](coord: YCoord,
                                       within: S): YCoord =
    YCoord(newPosition(coord.value, within))
}

object YSpeed {

  def Zero: YSpeed =
    YSpeed(SpeedValue.Zero)

  def RandomWithin(range: SpeedRange): YSpeed =
    YSpeed(
      SpeedValue.RandomBetween(range.min, range.max)
    )
}

case class ZSpeed(override val value: SpeedValue)
  extends Speed[ZCoord](value) {
  override def move[S <: Size[ZCoord]](coord: ZCoord,
                                       within: S): ZCoord =
    ZCoord(newPosition(coord.value, within))
}

object ZSpeed {

  def Zero: ZSpeed =
    ZSpeed(SpeedValue.Zero)

  def RandomWithin(range: SpeedRange): ZSpeed =
    ZSpeed(
      SpeedValue.RandomBetween(range.min, range.max)
    )
}