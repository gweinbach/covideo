/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Coord.*
import com.ezoky.ezgames.covideo.component.Dimension.*

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
sealed trait Size[C <: Coord](val value: SizeValue)(using val geometry: Geometry):

  def coord(position: PositionValue): C

  def relativePosition[N: Numeric](n: N): PositionValue =
    value.relativePosition(n)

  final def randomCoord: C =
    coord(value.randomPosition)

  final def maxCoord: C =
    coord(value.maxPosition)


case class Width(override val value: SizeValue)(using geometry: Geometry)
  extends Size[XCoord](value) :
  override def coord(position: PositionValue): XCoord =
    XCoord(position)


object Width:
  val Flat = Width(0 size)(using Geometry.Flat)


case class Height(override val value: SizeValue)(using geometry: Geometry)
  extends Size[YCoord](value) :
  override def coord(position: PositionValue): YCoord =
    YCoord(position)


object Height:
  val Flat = Height(0 size)(using Geometry.Flat)


case class Depth(override val value: SizeValue)(using geometry: Geometry)
  extends Size[ZCoord](value) :
  override def coord(position: PositionValue): ZCoord =
    ZCoord(position)


object Depth:
  val Flat = Depth(0 size)(using Geometry.Flat)
