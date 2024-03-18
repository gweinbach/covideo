/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Generate.*

import spire.*
import spire.implicits.*
import spire.math.*

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Coords[T: Dimension]:

  val CoordsDimension: Dimension[T] = summon[Dimension[T]]

  import CoordsDimension.*

  sealed trait Coord(value: PositionValue)

  final case class XCoord(value: PositionValue)
    extends Coord(value)

  final case class YCoord(value: PositionValue)
    extends Coord(value)

  final case class ZCoord(value: PositionValue)
    extends Coord(value)


  object XCoord:

    val Zero: XCoord =
      XCoord(PositionValue.Zero)

    def middle(x1: XCoord,
               x2: XCoord): XCoord =
      XCoord(PositionValue.middle(x1.value, x2.value))

//    def generatedWithin(width: Width): Generated[XCoord] =
//      width.generatedCoord


  object YCoord:

    val Zero: YCoord =
      YCoord(PositionValue.Zero)

    def middle(y1: YCoord,
               y2: YCoord): YCoord =
      YCoord(PositionValue.middle(y1.value, y2.value))

//    def generatedWithin(height: Height): Generated[YCoord] =
//      height.generatedCoord


  object ZCoord:

    val Zero: ZCoord =
      ZCoord(PositionValue.Zero)

    def middle(z1: ZCoord,
               z2: ZCoord): ZCoord =
      ZCoord(PositionValue.middle(z1.value, z2.value))

//    def generatedWithin(depth: Depth): Generated[ZCoord] =
//      depth.generatedCoord
  
  
