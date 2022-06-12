/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.*
import spire.*
import spire.algebra.Trig
import spire.implicits.*
import spire.math.*

import scala.reflect.Selectable.reflectiveSelectable

/**
 * @since 0.2.0
 * @author gweinbach on 06/06/2022
 */
trait Space[T: Numeric: Precision]:

  private val _SpatialNumeric: Numeric[T] = summon[Numeric[T]]

  private val _0: T = _SpatialNumeric.zero
  private val _1: T = _SpatialNumeric.one

  sealed trait Axis:
    def base: Vector

  object Axis:

    case object X extends Axis:
      override def base: Vector = Vector.OneX

    case object Y extends Axis:
      override def base: Vector = Vector.OneY

    case object Z extends Axis:
      override def base: Vector = Vector.OneZ

  case class Point(x: T,
                   y: T,
                   z: T)
    extends Transformable[Point]:

    infix def +(v: Vector): Point =
      Point(x + v.x, y + v.y, z + v.z)

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

  
  
  case class Vector(x: T,
                    y: T,
                    z: T)
    extends Transformable[Vector]:

    lazy val tuple: (T, T, T) =
      (x, y, z)

    lazy val magnitude: T =
      _SpatialNumeric.sqrt(this ⋅ this)

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

    infix def ⋅(v: Vector): T =
      x * v.x + y * v.y + z * v.z

    infix def ∧(v: Vector): Vector =
      Vector(
        y * v.z - z * v.y,
        z * v.x - x * v.z,
        x * v.y - y * v.x
      )

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

    def apply(magnitude: T,
              axis: Axis): Vector =
      axis match
        case Axis.X => Vector(magnitude, _0, _0)
        case Axis.Y => Vector(_0, magnitude, _0)
        case Axis.Z => Vector(_0, _0, magnitude)


    val Null = Vector(_0, _0, _0)

    val OneX = Vector(_1, _0, _0)
    val OneY = Vector(_0, _1, _0)
    val OneZ = Vector(_0, _0, _1)



  type Vertices = Iterable[Vertex]

  case class Vertex(s: Point,
                    t: Point)
    extends Transformable[Vertex]:

    infix def +(v: Vector): Vertex =
      Vertex(s + v, t + v)


  object Vertex:
    def apply(s: Point, v: Vector): Vertex =
      Vertex(s, v.dest(s))

