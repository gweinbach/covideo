/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import Dimension.*

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
enum Coord(value: PositionValue) {

  case XCoord(value: PositionValue)
    extends Coord(value)

  case YCoord(value: PositionValue)
    extends Coord(value)

  case ZCoord(value: PositionValue)
    extends Coord(value)

}
