/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Generate.*

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Position(x: XCoord,
                    y: YCoord,
                    z: ZCoord)

object Position:

  val Zero: Position =
    Position(XCoord.Zero, YCoord.Zero, ZCoord.Zero)

  def generatedWithin(width: Width,
                      height: Height,
                      depth: Depth): Generated[Position] =
    Generated.map3(
      XCoord.generatedWithin(width),
      YCoord.generatedWithin(height),
      ZCoord.generatedWithin(depth)
    )(Position(_, _, _))

  def generatedWithin(area: Box): Generated[Position] =
    generatedWithin(
      area.width,
      area.height,
      area.depth
    )

trait Positioned[T]:
  extension (positioned: T) 
    def position: Position