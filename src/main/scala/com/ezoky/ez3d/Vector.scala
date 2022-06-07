/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.Precisions
import spire.*
import spire.algebra.Trig
import spire.math.*
import spire.implicits.*
import com.ezoky.eznumber.Precisions

import reflect.Selectable.reflectiveSelectable

/**
 * @since 0.2.0
 * @author gweinbach on 06/06/2022
 */
trait Vectors[T: Fractional](spatialPrecision: T):

  type _SpatialCoordinate = T
  private val _SpatialFractional: Fractional[_SpatialCoordinate] = summon[Fractional[_SpatialCoordinate]]
  private val _0: _SpatialCoordinate = _SpatialFractional.zero
  private val _1: _SpatialCoordinate = _SpatialFractional.one

  val _Precisions: Precisions[T] = new Precisions[T] {}
  import _Precisions.*
  given SpatialPrecision: Precision = Precision(spatialPrecision)

  type RotationVector <: {
    def rotateVector(vector: Vector): Option[Vector]
  }

  sealed trait Axis:
    def base: Vector

  object Axis:

    case object X extends Axis:
      override def base: Vector = Vector.OneX

    case object Y extends Axis:
      override def base: Vector = Vector.OneY

    case object Z extends Axis:
      override def base: Vector = Vector.OneZ

  case class Point(x: _SpatialCoordinate,
                   y: _SpatialCoordinate,
                   z: _SpatialCoordinate):

    def translate(translationVector: Vector): Point =
      Point(x + translationVector.x, y + translationVector.y, z + translationVector.z)

    override def equals(obj: Any): Boolean =
      obj match
        case that: Point if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y) &&
            (this.z ~= that.z)
        case _ =>
          false


  object Point:
    val Zero = Point(_0, _0, _0)

    val OneX = Point(_1, _0, _0)
    val OneY = Point(_0, _1, _0)
    val OneZ = Point(_0, _0, _1)

  case class Vector(x: _SpatialCoordinate,
                    y: _SpatialCoordinate,
                    z: _SpatialCoordinate):

    lazy val tuple: (_SpatialCoordinate, _SpatialCoordinate, _SpatialCoordinate) =
      (x, y, z)

    lazy val magnitude: _SpatialCoordinate =
      _SpatialFractional.sqrt(this ⋅ this)

    lazy val normalized: Option[Vector] =
      this / magnitude

    def unary_- =
      Vector(-x, -y, -z)

    def dest(origin: Point = Point.Zero): Point =
      Point(origin.x + x, origin.y + y, origin.z + z)

    def +(v: Vector): Vector =
      Vector(x + v.x, y + v.y, z + v.z)

    def -(v: Vector): Vector =
      Vector(x - v.x, y - v.y, z - v.z)

    def *(n: T): Vector =
      Vector(
        x * n,
        y * n,
        z * n
      )

    def /(n: T): Option[Vector] =
      if n == _0 then
        None
      else
        Some(
          Vector(
            x / n,
            y / n,
            z / n
          )
        )

    infix def ⋅(v: Vector): _SpatialCoordinate =
      x * v.x + y * v.y + z * v.z

    infix def ∧(v: Vector): Vector =
      Vector(
        y * v.z - z * v.y,
        z * v.x - x * v.z,
        x * v.y - y * v.x
      )

    def rotate(rotationVector: RotationVector): Option[Vector] =
      rotationVector.rotateVector(this)

    override def equals(obj: Any): Boolean =
      obj match
        case that: Vector if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y) &&
            (this.z ~= that.z)
        case _ =>
          false


  object Vector:

    def apply(start: Point,
              end: Point): Vector =
      Vector(end.x - start.x, end.y - start.y, end.z - start.z)

    def apply(end: Point): Vector =
      Vector(Point.Zero, end)

    def apply(magnitude: _SpatialCoordinate,
              axis: Axis): Vector =
      axis match
        case Axis.X => Vector(magnitude, _0, _0)
        case Axis.Y => Vector(_0, magnitude, _0)
        case Axis.Z => Vector(_0, _0, magnitude)


    val Null = Vector(_0, _0, _0)

    val OneX = Vector(_1, _0, _0)
    val OneY = Vector(_0, _1, _0)
    val OneZ = Vector(_0, _0, _1)


  case class Vertex(s: Point,
                    t: Point):

    def translate(translationVector: Vector): Vertex =
      Vertex(s.translate(translationVector), t.translate(translationVector))

//    def rotate(rotationVector: Vector): Vertex =
//      Vertex(s.rotate(rotationVector), t.rotate(rotationVector))


  object Vertex:
    def apply(s: Point, v: Vector): Vertex =
      new Vertex(s, v.dest(s))

