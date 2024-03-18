/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Generate.Generated

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */

trait Boxes[T: Dimension]
  extends Positions[T]
    with Sizes[T]:

  import CoordsDimension.*

  case class Box(width: Width,
                 height: Height,
                 depth: Depth):

    lazy val minPosition: Position =
      Position(width.minCoord, height.minCoord, depth.minCoord)

    lazy val maxPosition: Position =
      Position(width.maxCoord, height.maxCoord, depth.maxCoord)

    lazy val center: Position =
      Position.middle(minPosition, maxPosition)

    def generatedPosition: Generated[Position] =
//      Position.generatedWithin(this)
  
//  extension (lhs: Position.type)
//    def generatedWithin(area: Box): Generated[Position] =
      Position.generatedWithin(
        width,
        height,
        depth
      )
