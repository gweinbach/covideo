/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*
import com.ezoky.ezgames.covideo.component.Generate.Generated
import spire.*
import spire.implicits.*
import spire.math.*

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
sealed trait Size[C <: Coord](val value: SizeValue)(using val geometry: Geometry):

  def coord(position: PositionValue): C

  private def relativePosition[N: Numeric](n: N): PositionValue =
    value.relativePosition(n)

  def *[N: Numeric](n: N): C =
    coord(relativePosition(n))

  def /[N: Numeric](n: N): Option[C] =
    val numericN = summon[Numeric[N]]
    if (numericN.zero == n)
      None
    else
      Some(coord(relativePosition(numericN.div(numericN.one, n))))

  @deprecated("Use generatedCoord instead")
  final def randomCoord: C =
    coord(value.randomPosition)

  final def minCoord: C =
    coord(value.minPosition)

  final def maxCoord: C =
    coord(value.maxPosition)

  final def generatedCoord: Generated[C] =
    GeneratedPositionValue.map(positionValue => coord(relativePosition(positionValue)(using NumericPositionValue)))

  // 3D extensions
  lazy val axis: Axis

  lazy val vector: Vector =
    value.axisVector(axis)

// end 3D extensions


case class Width(override val value: SizeValue)(using geometry: Geometry)
  extends Size[XCoord](value) :

  override def coord(position: PositionValue): XCoord =
    XCoord(position)

  override lazy val axis: Axis = Axis.X


object Width:
  val Flat = Width(0 size)(using Geometry.Flat)


case class Height(override val value: SizeValue)(using geometry: Geometry)
  extends Size[YCoord](value) :

  override def coord(position: PositionValue): YCoord =
    YCoord(position)

  override lazy val axis: Axis = Axis.Y


object Height:
  val Flat = Height(0 size)(using Geometry.Flat)


case class Depth(override val value: SizeValue)(using geometry: Geometry)
  extends Size[ZCoord](value) :

  override def coord(position: PositionValue): ZCoord =
    ZCoord(position)

  override lazy val axis: Axis = Axis.Z


object Depth:
  val Flat = Depth(0 size)(using Geometry.Flat)
