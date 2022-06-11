/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ez3d.{Double3D, Ez3D, H, Vectors}
import com.ezoky.eznumber.*

import scala.annotation.{tailrec, targetName}
import spire.*
import spire.algebra.{Order, Trig}
import spire.implicits.*
import spire.math.*

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
  private val _One: _DimensionType = 1.0
  private val _Epsilon: _DimensionType = 1E-16 // 17 significant digits for Double

  private def _Random: _DimensionType = Random().nextDouble()

  //  import Numeric.{DoubleIsFractional, Implicits, IntIsIntegral}

  private val _DimensionFractional: Fractional[_DimensionType] = summon[Fractional[_DimensionType]]
  private val _DimensionNumeric: Numeric[_DimensionType] = summon[Numeric[_DimensionType]]

  private def _NumberToDimensionConverter[N: Numeric]: (N) => _DimensionType =
    (n: N) => summon[Numeric[N]].toDouble(n)

  private val _GeneratedDimension: Generated[_DimensionType] = GeneratedDouble

  given Precision[_DimensionType] = Precision(1E-12)

  val Ez3D: Ez3D[_DimensionType] = new Double3D()
  import Ez3D.*

  // end of _DimensionType choice's dependencies


  enum Geometry:

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

    inline def isNull: Boolean =
      sizeValue == SizeValue.Zero

    inline def isNotNull: Boolean =
      !isNull

    def relativePosition[N: Numeric](n: N)(using geometry: Geometry): PositionValue =
      PositionValue(
        _DimensionFractional.times(sizeValue, _NumberToDimensionConverter.apply(n)),
        sizeValue,
        geometry
      )

    @targetName("zoom_SizeValue")
    def zoom[N: Numeric](ratio: N): SizeValue =
      _DimensionFractional.times(sizeValue, _NumberToDimensionConverter.apply(ratio))

    @deprecated("use GeneratedPositionValue instead")
    def randomPosition(using Geometry): PositionValue =
      relativePosition(_Random)

    def minPosition(using Geometry): PositionValue =
      relativePosition(0.0)

    def maxPosition(using Geometry): PositionValue =
      relativePosition(1.0 - _Epsilon)

    def isOutOfBounds(position: PositionValue): Boolean =
      (position < PositionValue.Zero) || (position >= sizeValue)

    inline def isWithinBounds(position: PositionValue): Boolean =
      !isOutOfBounds(position)

    def axisVector(axis: Axis): Vector =
      Vector(sizeValue, axis)

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

  val GeneratedSizeValue: Generated[SizeValue] = _GeneratedDimension

  val NumericSizeValue: Numeric[SizeValue] = _DimensionNumeric
  val FractionalSizeValue: Fractional[SizeValue] = _DimensionFractional


  // Position
  opaque type PositionValue = _DimensionType

  object PositionValue:

    val Zero: PositionValue = Dimension._Zero

    def apply(position: _DimensionType,
              withinBoundary: SizeValue,
              usingGeometry: Geometry): PositionValue =
      usingGeometry.normalizePosition(position, withinBoundary)

    @deprecated("Use Generated[PositionValue] instead")
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

    @targetName("zoom_PositionValue")
    def zoom[N: Numeric](ratio: N): PositionValue =
      _DimensionFractional.times(position, _NumberToDimensionConverter.apply(ratio))

    def move(speed: SpeedValue,
             withinBoundary: SizeValue,
             usingGeometry: Geometry): PositionValue =
      speed.on(position)(using withinBoundary, usingGeometry)

    def compare(otherPosition: PositionValue): Int =
      summon[Order[Double]].compare(position, otherPosition)


  import spire.compat.*

  given Ordering[PositionValue] =
    new Ordering[PositionValue] :
      override def compare(x: PositionValue,
                           y: PositionValue): Int = x.compare(y)

  val GeneratedPositionValue: Generated[PositionValue] = _GeneratedDimension

  val FractionalPositionValue: Fractional[PositionValue] = _DimensionFractional
  val NumericPositionValue: Numeric[PositionValue] = _DimensionNumeric


  // Timeval
  private type _StepType = Long

  opaque type DurationValue = _StepType

  object DurationValue:
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


  // Spatial Geometry

  //
  //  extension (t1: SpatialCoordinate)
  //    override def equals(obj: Any): Boolean =
  //      obj match
  //        case t2: SpatialCoordinate =>
  //          t1.~=(t2)(usinsuper.equals(obj)g precision)
  //        case _ =>
  //          false

  //  val SpatialPrecisions.OverrideEquals.*
  //
  //
  //  sealed trait Axis:
  //    def base: Vector
  //
  //  object Axis:
  //
  //    case object X extends Axis:
  //      override def base: Vector = Vector.OneX
  //
  //    case object Y extends Axis:
  //      override def base: Vector = Vector.OneY
  //
  //    case object Z extends Axis:
  //      override def base: Vector = Vector.OneZ
  //
  //  case class Point(x: SpatialCoordinate,
  //                   y: SpatialCoordinate,
  //                   z: SpatialCoordinate):
  //
  //    def translate(translationVector: Vector): Point =
  //      Point(x + translationVector.x, y + translationVector.y, z + translationVector.z)
  //
  //    override def equals(obj: Any): Boolean =
  //      obj match
  //        case that: Point if (that != null) =>
  //          (this.x ~= that.x) &&
  //          (this.y ~= that.y) &&
  //          (this.z ~= that.z)
  //        case _ =>
  //          false
  //
  //
  //  object Point:
  //    val Zero = Point(_Zero, _Zero, _Zero)
  //
  //    val OneX = Point(_One, _Zero, _Zero)
  //    val OneY = Point(_Zero, _One, _Zero)
  //    val OneZ = Point(_Zero, _Zero, _One)
  //
  //  case class Vector(x: SpatialCoordinate,
  //                    y: SpatialCoordinate,
  //                    z: SpatialCoordinate):
  //    lazy val dest: Point =
  //      Point(x, y, z)
  //
  //    lazy val tuple: (SpatialCoordinate, SpatialCoordinate, SpatialCoordinate) =
  //      (x, y, z)
  //
  //    def unary_- =
  //      Vector(-x, -y, -z)
  //
  //    def +(v: Vector): Vector =
  //      Vector(x + v.x, y + v.y, z + v.z)
  //
  //    def -(v: Vector): Vector =
  //      Vector(x - v.x, y - v.y, z - v.z)
  //
  //    def *[N: Numeric](n: N): Vector =
  //      Vector(
  //        _DimensionFractional.times(x, _NumberToDimensionConverter.apply(n)),
  //        _DimensionFractional.times(y, _NumberToDimensionConverter.apply(n)),
  //        _DimensionFractional.times(z, _NumberToDimensionConverter.apply(n))
  //      )
  //
  //    infix def ∧(v: Vector): Vector =
  //      Vector(
  //        y * v.z - z * v.y,
  //        z * v.x - x * v.z,
  //        x * v.y - y * v.x
  //      )
  //
  //    def rotate(rotationVector: RotationVector): Option[Vector] =
  //      rotationVector.rotateVector(this)
  //
  //    override def equals(obj: Any): Boolean =
  //      obj match
  //        case that: Vector if (that != null) =>
  //          (this.x ~= that.x) &&
  //          (this.y ~= that.y) &&
  //          (this.z ~= that.z)
  //        case _ =>
  //          false
  //
  //
  //  object Vector:
  //
  ////    def apply(tuple: (SpatialCoordinate, SpatialCoordinate, SpatialCoordinate)): Vector =
  ////      Vector(tuple._1, tuple._2, tuple._3)
  //
  //    def apply(start: Point,
  //              end: Point): Vector =
  //      Vector(end.x - start.x, end.y - start.y, end.z - start.z)
  //
  //    def apply(end: Point): Vector =
  //      Vector(Point.Zero, end)
  //
  //    def apply(sizeValue: SizeValue,
  //              axis: Axis): Vector =
  //      axis match
  //        case Axis.X => Vector(sizeValue, _Zero, _Zero)
  //        case Axis.Y => Vector(_Zero, sizeValue, _Zero)
  //        case Axis.Z => Vector(_Zero, _Zero, sizeValue)
  //
  //
  //    val Null = Vector(_Zero, _Zero, _Zero)
  //
  //    val OneX = Vector(_One, _Zero, _Zero)
  //    val OneY = Vector(_Zero, _One, _Zero)
  //    val OneZ = Vector(_Zero, _Zero, _One)
  //
  //
  //  case class Vertex(s: Point,
  //                    t: Point):
  //
  //    def translate(translationVector: Vector): Vertex =
  //      Vertex(s.translate(translationVector), t.translate(translationVector))

  //    def rotate(rotationVector: Vector): Vertex =
  //      Vertex(s.rotate(rotationVector), t.rotate(rotationVector))
  //
  //
  //  object Vertex:
  //    def apply(s: Point, v: Vector): Vertex =
  //      new Vertex(s, v.dest)


//  val H = new H[SpatialCoordinate] with Vectors[SpatialCoordinate](spatialPrecision = SpatialPrecision.value){}
//  import H.*
//
//  type Rotation = Quaternion
//
//  def Rotation(angle: SpatialCoordinate,
//               axis: Vector): Option[Rotation] =
//    Quaternion.fromRotationVector(axis, angle)
//
//  extension (rotationVector: RotationVector)
//    def rotateVector(vector: Vector): Option[Vector] =
//      rotationVector.rotate(vector.tuple).map(tuple => Vector(tuple._1, tuple._2, tuple._3))


