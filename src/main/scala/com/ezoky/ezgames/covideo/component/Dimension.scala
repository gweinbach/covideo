/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import Generate.*

import scala.annotation.tailrec
import scala.math.Numeric.DoubleIsFractional
import scala.util.Random

/**
 * @author gweinbach on 15/11/2020
 * @since 0.1.0
 */
object Dimension:

  // everything is this section depends on the choice made for _DimensionType
  // and must be updated if this changes
  private type _DimensionType = Double
  private val _Zero: _DimensionType = 0.0
  private val _Epsilon: _DimensionType = 1E-16 // 17 significant digits for Double

  private def _Random: _DimensionType = Random().nextDouble()

  import Numeric.{DoubleIsFractional, Implicits, IntIsIntegral}

  private val _DimensionFractional: Fractional[_DimensionType] = DoubleIsFractional

  private def _NumberToDimensionConverter[N: Numeric]: (N) => _DimensionType =
    (n: N) => summon[Numeric[N]].toDouble(n)

  // end of _DimensionType choice's dependencies


  enum Geometry :

    case Flat

    case Toric

    case Bounded

    private[Dimension] def normalizePosition(value: _DimensionType,
                                             boundary: SizeValue): _DimensionType =
      this match
        case Flat =>
          Dimension._Zero

        case Toric =>
          boundary.remainder(value)

        case Bounded =>
          boundary.bounce(value)




  opaque type SizeValue = _DimensionType

  object SizeValue:

    val Zero: SizeValue = Dimension._Zero

    def apply(size: _DimensionType): SizeValue = size.abs



  extension (sizeValue: SizeValue)

    def isNull: Boolean =
      sizeValue == SizeValue.Zero

    def isNotNull: Boolean =
      sizeValue != SizeValue.Zero

    def relativePosition[N: Numeric](n: N)(using geometry: Geometry): PositionValue =
      PositionValue(
        _DimensionFractional.times(sizeValue, _NumberToDimensionConverter.apply(n)),
        sizeValue,
        geometry
      )

    def zoom[N: Numeric](ratio: N): SizeValue =
      _DimensionFractional.times(sizeValue, _NumberToDimensionConverter.apply(ratio))

    def randomPosition(using Geometry): PositionValue =
      relativePosition(_Random)

    def minPosition(using Geometry): PositionValue =
      relativePosition(0.0)
      
    def maxPosition(using Geometry): PositionValue =
      relativePosition(1.0 - _Epsilon)

    def isOutOfBounds(position: PositionValue): Boolean =
      (position < PositionValue.Zero) || (position >= sizeValue)

    def isWithinBounds(position: PositionValue): Boolean =
      !isOutOfBounds(position)

    @tailrec
    private[Dimension] def remainder(dimensionValue: _DimensionType): _DimensionType =
      if (isNull) {
        Dimension._Zero
      }
      else if (dimensionValue < Dimension._Zero) {
        remainder(sizeValue + dimensionValue)
      }
      else {
        dimensionValue % sizeValue
      }

    @tailrec
    private[Dimension] def bounce(dimensionValue: _DimensionType): _DimensionType =
      if (isNull)
        Dimension._Zero
      else if (dimensionValue < Dimension._Zero)
        bounce(-dimensionValue)

      else if (dimensionValue >= sizeValue)
        val remainder = (dimensionValue - sizeValue) % sizeValue
        if (((dimensionValue - sizeValue) / sizeValue).floor.toInt % 2 == 0)
          sizeValue - remainder
        else
          remainder

      else
        dimensionValue


  // Position
  opaque type PositionValue = _DimensionType

  object PositionValue:

    val Zero: PositionValue = Dimension._Zero

    def apply(position: _DimensionType,
              withinBoundary: SizeValue,
              usingGeometry: Geometry): PositionValue =
      usingGeometry.normalizePosition(position, withinBoundary)

    def random(withinBoundary: SizeValue,
               usingGeometry: Geometry): PositionValue =
      withinBoundary.randomPosition(using usingGeometry)


  extension (position: PositionValue)

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


  given Ordering[PositionValue] =
    new Ordering[PositionValue]:
      override def compare(x: PositionValue,
                           y: PositionValue): Int = x.compare(y)


  // Timeval
  private type _StepType = Long

  opaque type DurationValue = _StepType

  object DurationValue :
    val Zero: DurationValue = 0

    def apply(duration: Long): DurationValue =
      duration.abs



  // Speed
  opaque type SpeedValue = _DimensionType

  object SpeedValue:

    val Zero: SpeedValue = Dimension._Zero

    def generatedBetween(min: SpeedValue,
                         max: SpeedValue): Generated[SpeedValue] =
      GeneratedDouble.map(d => min + (d * (max - min)))

    def apply(distance: _DimensionType,
              duration: DurationValue = 1): SpeedValue =
      distance / duration


  extension (speed: SpeedValue)

    def on(position: PositionValue)
          (using
           boundary: SizeValue,
           geometry: Geometry): PositionValue =
      PositionValue(position + speed, boundary, geometry)

    def accelerate(to: AccelerationValue): SpeedValue =
      to(speed)


  given Ordering[SpeedValue] =
    (x: SpeedValue, y: SpeedValue) => x.compare(y)



  // Acceleration
  opaque type AccelerationValue = _DimensionType

  object AccelerationValue:

    val Zero: AccelerationValue = Dimension._Zero

    def generatedBetween(min: AccelerationValue,
                         max: AccelerationValue): Generated[AccelerationValue] =
      GeneratedDouble.map(d => min + (d * (max - min)))

    def apply(acceleration: _DimensionType): AccelerationValue =
      acceleration


  extension (acceleration: AccelerationValue)
    def apply(speed: SpeedValue): SpeedValue =
      SpeedValue(speed + acceleration)


  given Ordering[AccelerationValue] =
    (x: AccelerationValue, y: AccelerationValue) => x.compare(y)


  /** Utilities methods added to [[_DimensionType]] */
  extension (dimensionValue: _DimensionType)

    def size: SizeValue =
      SizeValue(dimensionValue)

    def position(using boundary: SizeValue)(using geometry: Geometry): PositionValue =
      PositionValue(dimensionValue, boundary, geometry)

    def speed: SpeedValue =
      SpeedValue(dimensionValue)

    def acceleration: AccelerationValue =
      AccelerationValue(dimensionValue)


  /** Utilities methods added to [[_StepType]] */
  extension (durationValue: _StepType)

    def steps: DurationValue =
      DurationValue(durationValue)

