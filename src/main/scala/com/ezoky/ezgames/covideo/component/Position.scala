/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension.PositionValue

import Coord._

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Position1D(x: XCoord)

case class Position2D(x: XCoord,
                      y: YCoord)

case class Position3D(x: XCoord,
                      y: YCoord,
                      z: ZCoord)

enum Coord(value: PositionValue) {

  case XCoord(value: PositionValue)
    extends Coord(value)

  case YCoord(value: PositionValue)
    extends Coord(value)

  case ZCoord(value: PositionValue)
    extends Coord(value)

}
