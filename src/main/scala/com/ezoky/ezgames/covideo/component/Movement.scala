/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension.MovementValue

import Speed._

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Movement(xSpeed: XSpeed,
                    ySpeed: YSpeed,
                    ZSpeed: ZSpeed)

enum Speed(value: MovementValue) {

  case XSpeed(value: MovementValue)
    extends Speed(value)

  case YSpeed(value: MovementValue)
    extends Speed(value)

  case ZSpeed(value: MovementValue)
    extends Speed(value)

}