/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Generate.*

import scala.util.Random

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

  def generated(width: Width,
                height: Height,
                depth: Depth): Generated[Position] =
    Generated.map3(
      XCoord.generated(width),
      YCoord.generated(height),
      ZCoord.generated(depth)
    )(Position(_, _, _))

  def generated(area: Area): Generated[Position] =
    generated(
      area.width,
      area.height,
      area.depth
    )