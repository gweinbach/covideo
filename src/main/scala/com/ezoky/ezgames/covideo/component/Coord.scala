/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import Generate.*
import Dimension.*

import Numeric.Implicits._

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
sealed trait Coord(value: PositionValue)

final case class XCoord(value: PositionValue)
  extends Coord(value)

final case class YCoord(value: PositionValue)
  extends Coord(value)

final case class ZCoord(value: PositionValue)
  extends Coord(value)


object XCoord:

  val Zero: XCoord =
    XCoord(PositionValue.Zero)

  def generated(within: Width): Generated[XCoord] =
    generatedPositionValue(within).map(XCoord(_))


object YCoord:

  val Zero: YCoord =
    YCoord(PositionValue.Zero)

  def generated(within: Height): Generated[YCoord] =
    generatedPositionValue(within).map(YCoord(_))


object ZCoord:

  val Zero: ZCoord =
    ZCoord(PositionValue.Zero)

  def generated(within: Depth): Generated[ZCoord] =
    generatedPositionValue(within).map(ZCoord(_))


private def generatedPositionValue[C <: Coord, S <: Size[C]](size: S): Generated[PositionValue] =
  GeneratedDouble.map(size.relativePosition(_))

