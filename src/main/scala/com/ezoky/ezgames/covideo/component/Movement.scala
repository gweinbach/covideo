/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Movement(xSpeed: XSpeed,
                    ySpeed: YSpeed,
                    zSpeed: ZSpeed) {
  def move(position: Position,
           within: Area): Position =
    Position(
      xSpeed.move(position.x, within.width),
      ySpeed.move(position.y, within.height),
      zSpeed.move(position.z, within.depth),
    )
}

object Movement {

  def Zero: Movement =
    Movement(
      XSpeed.Zero,
      YSpeed.Zero,
      ZSpeed.Zero
    )
    
  def Random(xRange: SpeedRange,
             yRange: SpeedRange,
             zRange: SpeedRange): Movement =
    Movement(
      XSpeed.RandomWithin(xRange),
      YSpeed.RandomWithin(yRange),
      ZSpeed.RandomWithin(zRange)
    )
}
