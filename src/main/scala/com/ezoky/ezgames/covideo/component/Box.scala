/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Generate.Generated
import com.ezoky.ezgames.covideo.component.{Depth, Height, Width}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Box(width: Width,
               height: Height,
               depth: Depth):

  @deprecated("Use 'generatedPosition' instead")
  def randomPosition: Position =
    Position(width.randomCoord, height.randomCoord, depth.randomCoord)

  def minPosition: Position =
    Position(width.minCoord, height.minCoord, depth.minCoord)
    
  def maxPosition: Position =
    Position(width.maxCoord, height.maxCoord, depth.maxCoord)

  def generatedPosition: Generated[Position] =
    Position.generatedWithin(this)
