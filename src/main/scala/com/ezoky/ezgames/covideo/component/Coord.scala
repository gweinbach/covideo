/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import Dimension.*
import com.ezoky.ezgames.covideo.component.Coord.{XCoord, YCoord, ZCoord}
import Numeric.Implicits._

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
enum Coord(value: PositionValue):

  case XCoord(value: PositionValue)
    extends Coord(value)

  case YCoord(value: PositionValue)
    extends Coord(value)

  case ZCoord(value: PositionValue)
    extends Coord(value)


def generatedPositionValue[C <: Coord](size: Size[C]): Generated[PositionValue] =
  GeneratedDouble.map(size.relativePosition(_))

def generatedXCoord(width: Width): Generated[XCoord] =
  generatedPositionValue(width).map(XCoord(_))

def generatedYCoord(height: Height): Generated[YCoord] =
  generatedPositionValue(height).map(YCoord(_))

def generatedZCoord(depth: Depth): Generated[ZCoord] =
  generatedPositionValue(depth).map(ZCoord(_))
