/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import scala.annotation.tailrec

/**
 * @author gweinbach on 15/11/2020
 * @since 0.1.0
 */
object Dimension {

  private type _DimensionType = Double

  enum Geometry {

    case Toric

    case Bounded

    private[Dimension] def normalizePosition(value: _DimensionType,
                                             boundary: SizeValue): _DimensionType =
      this match {
        case Toric =>
          boundary.remainder(value)

        case Bounded =>
          boundary.bounce(value)
      }
  }


  opaque type SizeValue = _DimensionType

  object SizeValue {

    val Zero: SizeValue = 0.0

    def apply(size: _DimensionType): SizeValue = size.abs
  }


  import scala.math.Numeric.IntIsIntegral
  import scala.math.Numeric.DoubleIsFractional
  import Numeric.Implicits

  extension(sizeValue: SizeValue) {

    def isNull: Boolean =
      sizeValue == SizeValue.Zero

    def isNotNull: Boolean =
      sizeValue != SizeValue.Zero

    def isOutOfBounds(position: PositionValue): Boolean =
      (position < PositionValue.Zero) || (position >= sizeValue)

    @tailrec
    private[Dimension] def remainder(dimensionValue: _DimensionType): _DimensionType =
      if (isNull) {
        0.0
      }
      else if (dimensionValue < 0.0) {
        remainder(-dimensionValue)
      }
      else {
        dimensionValue % sizeValue
      }
    
    @tailrec
    private[Dimension] def bounce(dimensionValue: _DimensionType): _DimensionType =
      if (isNull) {
        0.0
      }
      else if (dimensionValue < 0) {
        bounce(-dimensionValue)
      }
      else if (dimensionValue >= sizeValue) {
        val remainder = (dimensionValue - sizeValue) % sizeValue
        if (((dimensionValue - sizeValue) / sizeValue).floor.toInt % 2 == 0) {
          sizeValue - remainder
        }
        else {
          remainder
        }
      }
      else {
        dimensionValue
      }
  }

  // Position
  opaque type PositionValue = _DimensionType

  object PositionValue {

    val Zero: PositionValue = 0.0

    def apply(position: _DimensionType,
              withinBoundary: SizeValue,
              usingGeometry: Geometry): PositionValue =
      usingGeometry.normalizePosition(position, withinBoundary)

  }

  extension(position: PositionValue) {

    def move(speed: SpeedValue,
             withinBoundary: SizeValue,
             usingGeometry: Geometry): PositionValue =
      speed.on(position)(using withinBoundary)(using usingGeometry)

    def compare(otherPosition: PositionValue): Int =
      summon[Ordering[Double]].compare(position, otherPosition)
  }

  given Ordering[PositionValue] =
    new Ordering[PositionValue] {
      override def compare(x: PositionValue,
                           y: PositionValue): Int = x.compare(y)
    }

  
  // Speed
  opaque type SpeedValue = _DimensionType

  object SpeedValue {
    val Zero: SpeedValue = 0.0
    
    def apply(speed: _DimensionType): SpeedValue =
      speed
  }

  extension(speed: SpeedValue) {

    def on(position: PositionValue)
          (using boundary: SizeValue)
          (using geometry: Geometry): PositionValue =
      PositionValue(position + speed, boundary, geometry)
      
    def accelerate(to: AccelerationValue): SpeedValue =
      to(speed)
  }

  
  // Acceleration
  opaque type AccelerationValue = _DimensionType
  
  object AccelerationValue {
    val Zero: AccelerationValue = 0.0

    def apply(acceleration: _DimensionType): AccelerationValue =
      acceleration
  }

  extension(acceleration: AccelerationValue) {

    def apply(speed: SpeedValue): SpeedValue =
      SpeedValue(speed + acceleration)
  }

  
  /** Utilities methods added to [[_DimensionType]] */
  extension(dimensionValue: _DimensionType) {

    def size: SizeValue =
      SizeValue(dimensionValue)

    def position(using boundary: SizeValue)(using geometry: Geometry): PositionValue =
      PositionValue(dimensionValue, boundary, geometry)
      
    def speed: SpeedValue =
      SpeedValue(dimensionValue)
      
    def acceleration: AccelerationValue =
      AccelerationValue(dimensionValue)
  }
}