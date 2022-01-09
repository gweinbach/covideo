/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import scala.annotation.tailrec
import scala.util.Random

/**
 * @author gweinbach on 15/11/2020
 * @since 0.1.0
 */
object Dimension {

  private type _DimensionType = Double
  private val _Zero: _DimensionType = 0.0
  private val _Epsilon: _DimensionType = 1E-16 // 17 significant digits for Double

  private def _Random: _DimensionType = Random().nextDouble()

  import Numeric.Implicits
  import scala.math.Numeric.{DoubleIsFractional, IntIsIntegral}

  private val _DimensionFractional: Fractional[_DimensionType] = DoubleIsFractional

  enum Geometry {

    case Flat

    case Toric

    case Bounded

    private[Dimension] def normalizePosition(value: _DimensionType,
                                             boundary: SizeValue): _DimensionType =
      this match {
        case Flat =>
          Dimension._Zero

        case Toric =>
          boundary.remainder(value)

        case Bounded =>
          boundary.bounce(value)
      }
  }


  opaque type SizeValue = _DimensionType

  object SizeValue {

    val Zero: SizeValue = Dimension._Zero

    def apply(size: _DimensionType): SizeValue = size.abs
  }


  extension (sizeValue: SizeValue) {

    def isNull: Boolean =
      sizeValue == SizeValue.Zero

    def isNotNull: Boolean =
      sizeValue != SizeValue.Zero

    def isOutOfBounds(position: PositionValue): Boolean =
      (position < PositionValue.Zero) || (position >= sizeValue)

    def isWithinBounds(position: PositionValue): Boolean =
      !isOutOfBounds(position)

    def randomPosition(using geometry: Geometry): PositionValue =
      PositionValue(
        _Random * sizeValue,
        sizeValue,
        geometry
      )

    def maxPosition(using geometry: Geometry): PositionValue =
      PositionValue(
        sizeValue * (1.0 - _Epsilon),
        sizeValue,
        geometry
      )

    @tailrec
    private[Dimension] def remainder(dimensionValue: _DimensionType): _DimensionType =
      if (isNull) {
        Dimension._Zero
      }
      else if (dimensionValue < Dimension._Zero) {
        remainder(-dimensionValue)
      }
      else {
        dimensionValue % sizeValue
      }

    @tailrec
    private[Dimension] def bounce(dimensionValue: _DimensionType): _DimensionType =
      if (isNull) {
        Dimension._Zero
      }
      else if (dimensionValue < Dimension._Zero) {
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

    val Zero: PositionValue = Dimension._Zero

    def apply(position: _DimensionType,
              withinBoundary: SizeValue,
              usingGeometry: Geometry): PositionValue =
      usingGeometry.normalizePosition(position, withinBoundary)

    def random(withinBoundary: SizeValue,
               usingGeometry: Geometry): PositionValue =
      withinBoundary.randomPosition(using usingGeometry)
  }

  extension (position: PositionValue) {

    def intValue: Int =
      _DimensionFractional.toInt(position)
    def longValue: Long =
      _DimensionFractional.toLong(position)
    def doubleValue: Double =
      _DimensionFractional.toDouble(position)

    def move(speed: SpeedValue,
             withinBoundary: SizeValue,
             usingGeometry: Geometry): PositionValue =
      speed.on(position)(using withinBoundary, usingGeometry)

    def compare(otherPosition: PositionValue): Int =
      summon[Ordering[Double]].compare(position, otherPosition)
  }

  given Ordering[PositionValue] =
    new Ordering[PositionValue] {
      override def compare(x: PositionValue,
                           y: PositionValue): Int = x.compare(y)
    }
  
  
  // Time
  private type _StepType = Long

  opaque type DurationValue = _StepType

  object DurationValue {
    val Zero: DurationValue = 0

    def apply(duration: Long): DurationValue =
      duration.abs
  }


  // Speed
  opaque type SpeedValue = _DimensionType

  object SpeedValue {

    val Zero: SpeedValue = Dimension._Zero

    def RandomBetween(min: SpeedValue,
                      max: SpeedValue): SpeedValue = 
      min + (Dimension._Random * (max - min))

    def apply(distance: _DimensionType,
              duration: DurationValue = 1): SpeedValue =
      distance / duration
  }

  extension (speed: SpeedValue) {

    def on(position: PositionValue)
          (using
           boundary: SizeValue,
           geometry: Geometry): PositionValue =
      PositionValue(position + speed, boundary, geometry)

    def accelerate(to: AccelerationValue): SpeedValue =
      to(speed)
  }

  given Ordering[SpeedValue] =
    new Ordering[SpeedValue] {
      override def compare(x: SpeedValue,
                           y: SpeedValue): Int = x.compare(y)
    }


  // Acceleration
  opaque type AccelerationValue = _DimensionType

  object AccelerationValue {
    val Zero: AccelerationValue = Dimension._Zero

    def apply(acceleration: _DimensionType): AccelerationValue =
      acceleration
  }

  extension (acceleration: AccelerationValue) {

    def apply(speed: SpeedValue): SpeedValue =
      SpeedValue(speed + acceleration)
  }


  /** Utilities methods added to [[_DimensionType]] */
  extension (dimensionValue: _DimensionType) {

    def size: SizeValue =
      SizeValue(dimensionValue)

    def position(using boundary: SizeValue)(using geometry: Geometry): PositionValue =
      PositionValue(dimensionValue, boundary, geometry)

    def speed: SpeedValue =
      SpeedValue(dimensionValue)

    def acceleration: AccelerationValue =
      AccelerationValue(dimensionValue)
  }

  /** Utilities methods added to [[_StepType]] */
  extension (durationValue: _StepType) {

    def steps: DurationValue =
      DurationValue(durationValue)
  }
}