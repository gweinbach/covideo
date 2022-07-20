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
abstract class Dimension[T: Precision: Numeric: Trig: Generated: Ez3D: Epsilon]:

  type DimensionBase = T

  // Depends on actual Dimension type and must be specialized therefore
  def modulo(a: T, b: T): T

  // rather use explicit conversion even if implicit one is private
  protected def _NumberToDimensionConverter[N: Numeric]: (N) => T

//  private given [N: Numeric]: Conversion[N, _DimensionType] with
//    def apply(n: N): _DimensionType = summon[Numeric[N]].toDouble(n)

  // end of Dimension type specific implementation

  
  // This means that _DimensionType is the same in 3D libraries
  val Ez3D: Ez3D[T] = summon[Ez3D[T]]
  import Ez3D.*

  private val _DimensionNumeric: Numeric[T] = summon[Numeric[T]]
  private val _DimensionOrder: Order[T] = summon[Order[T]]
  private val _GeneratedDimension: Generated[T] = summon[Generated[T]]
  
  private val _0: T = _DimensionNumeric.zero
  private val __1: T = _DimensionNumeric.one
  private val __2: T = _DimensionNumeric.fromInt(2)

  val Zero: DimensionBase = _0
  val One: DimensionBase = __1
  val Two: DimensionBase = __2

  enum Geometry:

    case Flat

    case Toric

    case Bounded

    private[Dimension] def normalizePosition(value: T,
                                             boundary: SizeValue): T =
      this match
        case Flat =>
          _0

        case Toric =>
          boundary.remainder(value)

        case Bounded =>
          boundary.bounce(value)


  // SizeValue
  opaque type SizeValue = T

  object SizeValue:

    val Zero: SizeValue = _0

    def apply(size: T): SizeValue = size.abs


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

    @targetName("sizeToBase")
    def baseValue: DimensionBase =
      sizeValue

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

    inline def minPosition(using Geometry): PositionValue =
      relativePosition(_0)

    inline def maxPosition(using geometry:  Geometry): PositionValue =
      geometry match
        case Geometry.Flat => minPosition
        case _ if (sizeValue == SizeValue.Zero) => minPosition
        case _ => relativePosition(__1 - ε[T])

    def isOutOfBounds(position: PositionValue): Boolean =
      (position < PositionValue.Zero) || (position >= sizeValue)

    inline def isWithinBounds(position: PositionValue): Boolean =
      !isOutOfBounds(position)

    def axisVector(axis: Axis): SpaceVector =
      SpaceVector(sizeValue, axis)

    private[Dimension] final def remainder(dimensionValue: T): T =
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
    private[Dimension] final def bounce(dimensionValue: T): T =
      if (isNull)
        _0
      else if (dimensionValue < _0)
        bounce(-dimensionValue)

      else if (dimensionValue >= sizeValue)
        val remainder = (dimensionValue - sizeValue) fmod sizeValue
        if (((dimensionValue - sizeValue) / sizeValue).floor.toInt % 2 == 0)
          sizeValue - remainder
        else
          remainder

      else
        dimensionValue

  val GeneratedSizeValue: Generated[SizeValue] = _GeneratedDimension

  val NumericSizeValue: Numeric[SizeValue] = _DimensionNumeric
  // end SizeValue


  // Position
  opaque type PositionValue = T

  object PositionValue:

    val Zero: PositionValue = _0

    def apply(position: T,
              withinBoundary: SizeValue,
              usingGeometry: Geometry): PositionValue =
      usingGeometry.normalizePosition(position, withinBoundary)

    def middle(p1: PositionValue,
               p2: PositionValue): PositionValue =
      (p1 + p2) / __2


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

    @targetName("positionToBase")
    def baseValue: DimensionBase =
      position

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
  opaque type SpeedValue = T

  object SpeedValue:

    val Zero: SpeedValue = _0

    def generatedBetween(min: SpeedValue,
                         max: SpeedValue): Generated[SpeedValue] =
      _GeneratedDimension.map(d => min + (d * (max - min)))

    def apply(distance: T,
              duration: DurationValue = DurationValue.One): SpeedValue =
      distance / _NumberToDimensionConverter(using Numeric[DurationValue])(duration)


  extension (speed: SpeedValue)

    def on(position: PositionValue)
          (using
           boundary: SizeValue,
           geometry: Geometry): PositionValue =
      PositionValue(position + speed, boundary, geometry)

    def accelerate(to: AccelerationValue): SpeedValue =
      SpeedValue(speed + to)


  given Order[SpeedValue] =
    (x: SpeedValue, y: SpeedValue) => x.compare(y)
  // end SpeedValue

  // AccelerationValue
  opaque type AccelerationValue = T

  object AccelerationValue:

    val Zero: AccelerationValue = _0

    def generatedBetween(min: AccelerationValue,
                         max: AccelerationValue): Generated[AccelerationValue] =
      _GeneratedDimension.map(d => min + (d * (max - min)))

    def apply(acceleration: T): AccelerationValue =
      acceleration

  extension (acceleration: AccelerationValue)
    def apply(speed: SpeedValue): SpeedValue =
      speed.accelerate(acceleration)


  given Order[AccelerationValue] =
    (x: AccelerationValue, y: AccelerationValue) => x.compare(y)
  // end AccelerationValue

  // Spin
  opaque type SpinValue = T

  object SpinValue:

    val Zero: SpinValue = _0

    def generatedBetween(min: SpinValue,
                         max: SpinValue): Generated[SpinValue] =
      _GeneratedDimension.map(d => min + (d * (max - min)))

    def apply(distance: T,
              duration: DurationValue = DurationValue.One): SpinValue =
      distance / _NumberToDimensionConverter(using Numeric[DurationValue])(duration)


  extension (spin: SpinValue)

    @targetName("spinToBase")
    def baseValue: DimensionBase =
      spin

  //    def on(position: PositionValue)
//          (using
//           boundary: SizeValue,
//           geometry: Geometry): PositionValue =
//      PositionValue(position + spin, boundary, geometry)

    def angularAccelerate(to: AngularAccelerationValue): SpinValue =
      SpinValue(spin + to)


  given Order[SpinValue] =
    (x: SpinValue, y: SpinValue) => x.compare(y)
  // end SpinValue


  // AngularAccelerationValue
  opaque type AngularAccelerationValue = T

  object AngularAccelerationValue:

    val Zero: AngularAccelerationValue = _0

    def generatedBetween(min: AngularAccelerationValue,
                         max: AngularAccelerationValue): Generated[AngularAccelerationValue] =
      _GeneratedDimension.map(d => min + (d * (max - min)))

    def apply(angularAcceleration: T): AngularAccelerationValue =
      angularAcceleration


  extension (angularAcceleration: AngularAccelerationValue)
    @targetName("angularAccelerateSpin")
    def apply(spin: SpinValue): SpinValue =
      spin.angularAccelerate(angularAcceleration)


  given Order[AngularAccelerationValue] =
    (x: AngularAccelerationValue, y: AngularAccelerationValue) => x.compare(y)
  // end AngularAccelerationValue

  
  /** Utilities methods added to [[T]] */
  extension (lhs: T)

    // % operator is not defined for floating numbers in spire
    infix def fmod(rhs: T): T =
      modulo(lhs, rhs)

    @targetName("baseToSize")
    def size: SizeValue =
      SizeValue(lhs)

    def position(using boundary: SizeValue)(using geometry: Geometry): PositionValue =
      PositionValue(lhs, boundary, geometry)

    def speed: SpeedValue =
      SpeedValue(lhs)

    def acceleration: AccelerationValue =
      AccelerationValue(lhs)

    def spin: SpinValue =
      SpinValue(lhs)
    
    def angularAcceleration: AngularAccelerationValue =
      AngularAccelerationValue(lhs)


  /** Utilities methods added to [[_StepType]] */
  extension (durationValue: _StepType)

    def steps: DurationValue =
      DurationValue(durationValue)



given Precision[Double] = Precision(1E-10d)

object Dimension extends Dimension[Double]:

  override def modulo(a: Double, b: Double): Double = a % b

  final override protected def _NumberToDimensionConverter[N: Numeric]: (N) => Double =
    (n: N) => summon[Numeric[N]].toDouble(n)

