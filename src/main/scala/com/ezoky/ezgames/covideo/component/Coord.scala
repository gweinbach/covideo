/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Generate.*

import scala.math.Numeric.Implicits.*

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

  def generatedWithin(width: Width): Generated[XCoord] =
    width.generatedCoord


object YCoord:

  val Zero: YCoord =
    YCoord(PositionValue.Zero)

  def generatedWithin(height: Height): Generated[YCoord] =
    height.generatedCoord


object ZCoord:

  val Zero: ZCoord =
    ZCoord(PositionValue.Zero)

  def generatedWithin(depth: Depth): Generated[ZCoord] =
    depth.generatedCoord


