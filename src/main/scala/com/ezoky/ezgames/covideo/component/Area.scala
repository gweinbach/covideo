/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.{Height, Width, Depth}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Area(width: Width,
                height: Height,
                depth: Depth) {

  def randomPosition: Position =
    Position(width.randomCoord, height.randomCoord, depth.randomCoord)

  def maxPosition: Position =
    Position(width.maxCoord, height.maxCoord, depth.maxCoord)
}
