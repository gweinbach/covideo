/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Generate.{*, given}
import com.ezoky.ez3d.{Ez3D, given}
import com.ezoky.eznumber.{Epsilon, Precision, ε, given}

import scala.annotation.{tailrec, targetName}
import spire.*
import spire.algebra.{EuclideanRing, Order, Trig}
import spire.implicits.*
import spire.math.*

import scala.math.Integral.Implicits.infixIntegralOps
import scala.util.Random

/**
 * @author gweinbach on 15/11/2020
 * @since 0.1.0
 */
abstract class Dimension[_DimensionType: Precision: Numeric: Trig: Generated: Ez3D: Epsilon]:

  type DimensionBase = _DimensionType

  // everything is this section depends on the choice made for _DimensionType
  // and must be updated if this changes
//  private type _DimensionType = Double
//  private val _DimensionNumeric: Fractional[_DimensionType] = summon[Fractional[_DimensionType]]
  private val _DimensionNumeric: Numeric[_DimensionType] = summon[Numeric[_DimensionType]]
  private val _DimensionOrder: Order[_DimensionType] = summon[Order[_DimensionType]]
//  private val _Epsilon: _DimensionType = 1E-16 // 17 significant digits for Double

//  private def _Random: _DimensionType = Random().nextDouble()

  // rather use explicit conversion even if implicit one is private
  protected def _NumberToDimensionConverter[N: Numeric]: (N) => _DimensionType

//  private given [N: Numeric]: Conversion[N, _DimensionType] with
//    def apply(n: N): _DimensionType = summon[Numeric[N]].toDouble(n)

  private val _GeneratedDimension: Generated[_DimensionType] = summon[Generated[_DimensionType]]

//  given Precision[_DimensionType] = Precision(1E-12)

  // This means that _DimensionType is the same in 3D libraries
  val Ez3D: Ez3D[_DimensionType] = summon[Ez3D[_DimensionType]]
  import Ez3D.*

  // end of _DimensionType choice's dependencies

  private val _0: _DimensionType = _DimensionNumeric.zero
  private val __1: _DimensionType = _DimensionNumeric.one

  val Zero = _0
  val One = __1

  enum Geometry:

    case Flat

    case Toric

    case Bounded

    private[Dimension] def normalizePosition(value: _DimensionType,
                                             boundary: SizeValue): _DimensionType =
      this match
        case Flat =>
          _0

        case Toric =>
          boundary.remainder(value)

        case Bounded =>
          boundary.bounce(value)


  opaque type SizeValue = _DimensionType

  object SizeValue:

    val Zero: SizeValue = _0

    def apply(size: _DimensionType): SizeValue = size.abs


  extension (sizeValue: SizeValue)

    inline def isNull: Boolean =
      sizeValue == SizeValue.Zero

    inline def isNotNull: Boolean =
      !isNull

    @targetName("sizeToInt")
    def intValue: Int =
      _DimensionNumeric.toInt(sizeValue)

    @targetName("sizeToLong")
    def longValue: Long =
      _DimensionNumeric.toLong(sizeValue)

    @targetName("sizeToDouble")
    def doubleValue: Double =
      _DimensionNumeric.toDouble(sizeValue)

    def relativePosition[N: Numeric](n: N)
                                    (using geometry: Geometry): PositionValue =
      PositionValue(
        sizeValue * _NumberToDimensionConverter.apply(n),
//        sizeValue * n,
        sizeValue,
        geometry
      )

    @targetName("zoom_SizeValue")
    def zoom[N: Numeric](ratio: N): SizeValue =
      sizeValue * _NumberToDimensionConverter.apply(ratio)
//      sizeValue * ratio

//    @deprecated("use GeneratedPositionValue instead")
//    def randomPosition(using Geometry): PositionValue =
//      relativePosition(_Random)

    inline def minPosition(using Geometry): PositionValue =
      relativePosition(_0)

    inline def maxPosition(using geometry:  Geometry): PositionValue =
      geometry match
        case Geometry.Flat => minPosition
        case _ if (sizeValue == SizeValue.Zero) => minPosition
        case _ => relativePosition(__1 - ε[_DimensionType])

    def isOutOfBounds(position: PositionValue): Boolean =
      (position < PositionValue.Zero) || (position >= sizeValue)

    inline def isWithinBounds(position: PositionValue): Boolean =
      !isOutOfBounds(position)

    def axisVector(axis: Axis): SpaceVector =
      SpaceVector(sizeValue, axis)

    private[Dimension] final def remainder(dimensionValue: _DimensionType): _DimensionType =
      if (isNull) {
        _0
      }
      else if (dimensionValue < _0) {
        (sizeValue + (dimensionValue fmod sizeValue)) fmod sizeValue
      }
      else {
        dimensionValue fmod sizeValue
      }

    @tailrec
    private[Dimension] final def bounce(dimensionValue: _DimensionType): _DimensionType =
      if (isNull)
        _0
      else if (dimensionValue < _0)
        bounce(-dimensionValue)

      else if (dimensionValue >= sizeValue)
        val remainder = (dimensionValue - sizeValue) fmod sizeValue
        println(s"dimensionValue=$dimensionValue")
        println(s"sizeValue=$sizeValue")
        println(s"(dimensionValue - sizeValue)=${(dimensionValue - sizeValue)}")
        println(s"remainder=$remainder")
        if (((dimensionValue - sizeValue) / sizeValue).floor.toInt % 2 == 0)
          println(s"sizeValue - remainder=${sizeValue - remainder}")
          sizeValue - remainder
        else
          remainder

      else
        dimensionValue

  val GeneratedSizeValue: Generated[SizeValue] = _GeneratedDimension

  val NumericSizeValue: Numeric[SizeValue] = _DimensionNumeric


  // Position
  opaque type PositionValue = _DimensionType

  object PositionValue:

    val Zero: PositionValue = _0

    def apply(position: _DimensionType,
              withinBoundary: SizeValue,
              usingGeometry: Geometry): PositionValue =
      usingGeometry.normalizePosition(position, withinBoundary)

//    @deprecated("Use Generated[PositionValue] instead")
//    def random(withinBoundary: SizeValue,
//               usingGeometry: Geometry): PositionValue =
//      withinBoundary.randomPosition(using usingGeometry)


  extension (position: PositionValue)

    @targetName("positionToInt")
    def intValue: Int =
      _DimensionNumeric.toInt(position)

    @targetName("positionToLong")
    def longValue: Long =
      _DimensionNumeric.toLong(position)

    @targetName("positionToDouble")
    def doubleValue: Double =
      _DimensionNumeric.toDouble(position)

    @targetName("zoom_PositionValue")
    def zoom[N: Numeric](ratio: N): PositionValue =
      position * _NumberToDimensionConverter.apply(ratio)
//      position * ratio

    def move(speed: SpeedValue,
             withinBoundary: SizeValue,
             usingGeometry: Geometry): PositionValue =
      speed.on(position)(using withinBoundary, usingGeometry)

    def compare(otherPosition: PositionValue): Int =
      _DimensionOrder.compare(position, otherPosition)


  given Order[PositionValue] =
    new Order[PositionValue] :
      override def compare(x: PositionValue,
                           y: PositionValue): Int = _DimensionOrder.compare(x, y)

  val GeneratedPositionValue: Generated[PositionValue] = _GeneratedDimension

  val NumericPositionValue: Numeric[PositionValue] = _DimensionNumeric

  val OrderPositionValue: Order[PositionValue] = _DimensionOrder


  // Timeval
  type _StepType = Long

  opaque type DurationValue = _StepType

  object DurationValue:
    val Zero: DurationValue = 0
    val One: DurationValue = 1

    def apply(duration: Long): DurationValue =
      Numeric[Long].abs(duration)


  // Speed
  opaque type SpeedValue = _DimensionType

  object SpeedValue:

    val Zero: SpeedValue = _0

    def generatedBetween(min: SpeedValue,
                         max: SpeedValue): Generated[SpeedValue] =
      _GeneratedDimension.map(d => min + (d * (max - min)))

    def apply(distance: _DimensionType,
              duration: DurationValue = DurationValue.One): SpeedValue =
      distance / _NumberToDimensionConverter(using Numeric[DurationValue])(duration)


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

    val Zero: AccelerationValue = _0

    def generatedBetween(min: AccelerationValue,
                         max: AccelerationValue): Generated[AccelerationValue] =
      _GeneratedDimension.map(d => min + (d * (max - min)))

    def apply(acceleration: _DimensionType): AccelerationValue =
      acceleration


  extension (acceleration: AccelerationValue)
    def apply(speed: SpeedValue): SpeedValue =
      SpeedValue(speed + acceleration)


  given Ordering[AccelerationValue] =
    (x: AccelerationValue, y: AccelerationValue) => x.compare(y)


  def modulo(a: _DimensionType, b: _DimensionType): _DimensionType

  /** Utilities methods added to [[_DimensionType]] */
  extension (lhs: _DimensionType)

    // % operqtor is not defined for floating numbers in spire
    infix def fmod(rhs: _DimensionType): _DimensionType =
      modulo(lhs, rhs)

    def size: SizeValue =
      SizeValue(lhs)

    def position(using boundary: SizeValue)(using geometry: Geometry): PositionValue =
      PositionValue(lhs, boundary, geometry)

    def speed: SpeedValue =
      SpeedValue(lhs)

    def acceleration: AccelerationValue =
      AccelerationValue(lhs)

  /** Utilities methods added to [[_StepType]] */
  extension (durationValue: _StepType)

    def steps: DurationValue =
      DurationValue(durationValue)

given Precision[Double] = Precision(1E-10d)

object Dimension extends Dimension[Double]:

  override def modulo(a: Double, b: Double): Double = a % b

  final override protected def _NumberToDimensionConverter[N: Numeric]: (N) => Double =
    (n: N) => summon[Numeric[N]].toDouble(n)

