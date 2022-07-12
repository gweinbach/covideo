/*
 * @author gweinbach on 14/06/2022 22:55
 * @since 0.2.0
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.*
import spire.*
import spire.algebra.Trig
import spire.implicits.*
import spire.math.*

import scala.None
import scala.annotation.targetName

/**
 * @since 0.2.0
 * @author gweinbach on 14/06/2022
 */
trait Transformation2D[T: Numeric : Trig : Precision]
  extends Plane[T]:

  private val _Numeric = Numeric[T]
  private val _0: T = _Numeric.zero
  private val __1: T = _Numeric.one // double '_' to avoid conflict with Product<X>._1

  case class HVector2D(x: T,
                       y: T,
                       w: T):

    lazy val cartesian: Option[PlanePoint] =
      if isAtInfinity then
        None
      else
        Some(PlanePoint(x / w, y / w))

    lazy val isAtInfinity: Boolean =
      w == 0

    def ×(m: Matrix2D): HVector2D =
      HVector2D(
        x * m.x00 + y * m.x10 + w * m.x20,
        x * m.x01 + y * m.x11 + w * m.x21,
        x * m.x02 + y * m.x12 + w * m.x22
      )
    
    override def equals(obj: Any): Boolean =
      obj match
        case that: HVector2D if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y) &&
            (this.w ~= that.w)
        case _ =>
          false

  object HVector2D:
    def point(p: PlanePoint): HVector2D =
      HVector2D(p.x, p.y,  __1)

    def vector(v: PlaneVector): HVector2D =
      HVector2D(v.x, v.y, _0)

    val Zero = HVector2D(_0, _0, _0)
    val One = HVector2D(__1, __1, __1)
  

  extension (o: Object2D)
    def homogeneous: HVector2D =
      o match
        case p: PlanePoint =>
          HVector2D.point(p)
        case v:PlaneVector =>
          HVector2D.vector(v)

  /**
   * We use Column major convention
   */
  trait Matrix2D:
    val x00, x01, x02: T
    val x10, x11, x12: T
    val x20, x21, x22: T

    /**
     * the trailling colon is to enforce right associativity (only operators
     * ending with a colon are right associative in scala) required by
     * Column mqjor convention.
     */
    @targetName("times")
    infix def ×:(m: Matrix2D): Matrix2D =
      Matrix2D(
        y00 = x00 * m.x00 + x01 * m.x10 + x02 * m.x20,
        y01 = x00 * m.x01 + x01 * m.x11 + x02 * m.x21,
        y02 = x00 * m.x02 + x01 * m.x12 + x02 * m.x22,

        y10 = x10 * m.x00 + x11 * m.x10 + x12 * m.x20,
        y11 = x10 * m.x01 + x11 * m.x11 + x12 * m.x21,
        y12 = x10 * m.x02 + x11 * m.x12 + x12 * m.x22,

        y20 = x20 * m.x00 + x21 * m.x10 + x22 * m.x20,
        y21 = x20 * m.x01 + x21 * m.x11 + x22 * m.x21,
        y22 = x20 * m.x02 + x21 * m.x12 + x22 * m.x22,
      )

    @targetName("add")
    infix def +(m: Matrix2D): Matrix2D =
      Matrix2D(
        y00 = x00 * m.x00,
        y01 = x01 * m.x01,
        y02 = x02 * m.x02,

        y10 = x10 * m.x10,
        y11 = x11 * m.x11,
        y12 = x12 * m.x12,

        y20 = x20 * m.x20,
        y21 = x21 * m.x21,
        y22 = x22 * m.x22,
      )

    def ×(v: HVector2D): HVector2D =
      HVector2D(
        x00 * v.x + x01 * v.y + x02 * v.w,
        x10 * v.x + x11 * v.y + x12 * v.w,
        x20 * v.x + x21 * v.y + x22 * v.w
      )

    lazy val transpose: Matrix2D =
      Matrix2D(
        y00 = x00,
        y01 = x10,
        y02 = x20,

        y10 = x01,
        y11 = x11,
        y12 = x21,

        y20 = x02,
        y21 = x12,
        y22 = x22,
      )
      

    final override def equals(obj: Any): Boolean =
      obj match
        case that: Matrix2D if (that != null) =>
          (this.x00 ~= that.x00) &&
            (this.x01 ~= that.x01) &&
            (this.x02 ~= that.x02) &&

          (this.x10 ~= that.x10) &&
            (this.x11 ~= that.x11) &&
            (this.x12 ~= that.x12) &&

          (this.x20 ~= that.x20) &&
            (this.x21 ~= that.x21) &&
            (this.x22 ~= that.x22)
        case _ =>
          false


  object Matrix2D:
    def apply(y00: T, y01: T, y02: T,
              y10: T, y11: T, y12: T,
              y20: T, y21: T, y22: T): Matrix2D =
      new Matrix2D {
        override val x00 = y00
        override val x01 = y01
        override val x02 = y02

        override val x10 = y10
        override val x11 = y11
        override val x12 = y12

        override val x20 = y20
        override val x21 = y21
        override val x22 = y22
      }

    def diagonal(x0: T,
                 x1: T,
                 x2: T) =
      Matrix2D(
        y00 = x0,
        y01 = _0,
        y02 = _0,

        y10 = _0,
        y11 = x1,
        y12 = _0,

        y20 = _0,
        y21 = _0,
        y22 = x2
      )

    val Identity =
      diagonal(__1, __1, __1)

  extension (matrix: Matrix2D)

    def apply(point: PlanePoint): Option[PlanePoint] =
      (matrix × point.homogeneous).cartesian

    def apply(vertex: PlaneVertex): Option[PlaneVertex] =
      for
        s <- matrix(vertex.s)
        t <- matrix(vertex.t)
      yield
        PlaneVertex(s, t)

  
  // Some 2D trqnsformations
  
  trait AffineTransformation2D extends Matrix2D :
    final override val x20: T = _0
    final override val x21: T = _0
    final override val x22: T = __1

//  case class Homothety2D(ratio: T)
//    extends AffineTransformation2D :
//    override val x00 = ratio
//    override val x01 = _0
//    override val x02 = _0
//
//    override val x10 = _0
//    override val x11 = ratio
//    override val x12 = _0
//
//    def inverse: Option[Homothety2D] =
//      if ratio == _0 then
//        None
//      else
//        Some(Homothety2D(_1 / ratio))

  case class Scaling2D(ratio: PlaneVector)
    extends AffineTransformation2D :
    override val x00 = ratio.x
    override val x01 = _0
    override val x02 = _0

    override val x10 = _0
    override val x11 = ratio.y
    override val x12 = _0

    def inverse: Option[Scaling2D] =
      ratio.inverse.map(Scaling2D(_))

  object Scaling2D:
    def isotropic(ratio: T): Scaling2D =
      Scaling2D(PlaneVector.fill(ratio))


  case class Translation2D(vector: PlaneVector)
    extends AffineTransformation2D :
    override val x00 = __1
    override val x01 = _0
    override val x02 = vector.x

    override val x10 = _0
    override val x11 = __1
    override val x12 = vector.y

    def inverse: Translation2D =
      Translation2D(-vector)



  trait AffineRotation2D
    extends AffineTransformation2D:
    override val x02 = _0
    override val x12 = _0


  case class BasisTransformation2D(basis: Basis2D)
    extends AffineRotation2D:
    override val x00 = basis.i.x
    override val x01 = basis.i.y

    override val x10 = basis.j.x
    override val x11 = basis.j.y

    def inverse: Matrix2D =
      transpose


//  trait AxisRotation
//    extends AffineRotation2D:
//    val angle: T
//
//  trait BaseAxisRotation
//    extends AxisRotation :
//    val axis: Axis
//
//  case class OxRotation(angle: T)
//    extends BaseAxisRotation :
//    override val axis: Axis = Axis.X
//
//    override val x00 = __1
//    override val x01 = _0
//    override val x02 = _0
//
//    override val x10 = _0
//    override val x11 = cos(angle)
//    override val x12 = sin(angle)
//
//    override val x20 = _0
//    override val x21 = -sin(angle)
//    override val x22 = cos(angle)
//
//  case class OyRotation(angle: T)
//    extends BaseAxisRotation :
//    override val axis: Axis = Axis.Y
//
//    override val x00 = cos(angle)
//    override val x01 = _0
//    override val x02 = -sin(angle)
//
//    override val x10 = _0
//    override val x11 = __1
//    override val x12 = _0
//
//    override val x20 = sin(angle)
//    override val x21 = _0
//    override val x22 = cos(angle)
//
//  case class OzRotation(angle: T)
//    extends BaseAxisRotation :
//    override val axis: Axis = Axis.Z
//
//    override val x00 = cos(angle)
//    override val x01 = sin(angle)
//    override val x02 = _0
//
//    override val x10 = -sin(angle)
//    override val x11 = cos(angle)
//    override val x12 = _0
//
//    override val x20 = _0
//    override val x21 = _0
//    override val x22 = __1
//
