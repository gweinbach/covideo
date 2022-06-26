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
trait Transformation3D[T: Numeric : Trig : Precision]
  extends Space[T] with H[T] with Angles[T]:

  private val _Numeric = Numeric[T]
  private val _0: T = _Numeric.zero
  private val __1: T = _Numeric.one // double '_' to avoid conflict with Product<X>._1
  private val __2: T = _Numeric.fromInt(2) // double '_' to avoid conflict with Product<X>._1

  case class HVector3D(x: T,
                       y: T,
                       z: T,
                       w: T):

    lazy val cartesian: Option[SpacePoint] =
      if isAtInfinity then
        None
      else
        Some(SpacePoint(x / w, y / w, z / w))

    lazy val isAtInfinity: Boolean =
      w == 0

    def ×(m: Matrix): HVector3D =
      HVector3D(
        x * m.x00 + y * m.x10 + z * m.x20 + w * m.x30,
        x * m.x01 + y * m.x11 + z * m.x21 + w * m.x31,
        x * m.x02 + y * m.x12 + z * m.x22 + w * m.x32,
        x * m.x03 + y * m.x13 + z * m.x23 + w * m.x33
      )
    
    override def equals(obj: Any): Boolean =
      obj match
        case that: HVector3D if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y) &&
            (this.z ~= that.z) &&
            (this.w ~= that.w)
        case _ =>
          false

  object HVector3D:
    def point(p: SpacePoint): HVector3D =
      HVector3D(p.x, p.y, p.z, __1)

    def vector(v: SpaceVector): HVector3D =
      HVector3D(v.x, v.y, v.z, _0)

    val Zero = HVector3D(_0, _0, _0, _0)
    val One = HVector3D(__1, __1, __1, __1)


  extension (o: Object3D)
    def homogeneous: HVector3D =
      o match
        case p: SpacePoint =>
          HVector3D.point(p)
        case v:SpaceVector =>
          HVector3D.vector(v)

  /**
   * We use Column major convention
   */
  trait Matrix:
    val x00, x01, x02, x03: T
    val x10, x11, x12, x13: T
    val x20, x21, x22, x23: T
    val x30, x31, x32, x33: T

    /**
     * the trailling colon is to enforce right associativity (only operators
     * ending with a colon are right associative in scala) required by
     * Column mqjor convention.
     */
    @targetName("times")
    infix def ×:(m: Matrix): Matrix =
      Matrix(
        y00 = x00 * m.x00 + x01 * m.x10 + x02 * m.x20 + x03 * m.x30,
        y01 = x00 * m.x01 + x01 * m.x11 + x02 * m.x21 + x03 * m.x31,
        y02 = x00 * m.x02 + x01 * m.x12 + x02 * m.x22 + x03 * m.x32,
        y03 = x00 * m.x03 + x01 * m.x13 + x02 * m.x23 + x03 * m.x33,

        y10 = x10 * m.x00 + x11 * m.x10 + x12 * m.x20 + x13 * m.x30,
        y11 = x10 * m.x01 + x11 * m.x11 + x12 * m.x21 + x13 * m.x31,
        y12 = x10 * m.x02 + x11 * m.x12 + x12 * m.x22 + x13 * m.x32,
        y13 = x10 * m.x03 + x11 * m.x13 + x12 * m.x23 + x13 * m.x33,

        y20 = x20 * m.x00 + x21 * m.x10 + x22 * m.x20 + x23 * m.x30,
        y21 = x20 * m.x01 + x21 * m.x11 + x22 * m.x21 + x23 * m.x31,
        y22 = x20 * m.x02 + x21 * m.x12 + x22 * m.x22 + x23 * m.x32,
        y23 = x20 * m.x03 + x21 * m.x13 + x22 * m.x23 + x23 * m.x33,

        y30 = x30 * m.x00 + x31 * m.x10 + x32 * m.x20 + x33 * m.x30,
        y31 = x30 * m.x01 + x31 * m.x11 + x32 * m.x21 + x33 * m.x31,
        y32 = x30 * m.x02 + x31 * m.x12 + x32 * m.x22 + x33 * m.x32,
        y33 = x30 * m.x03 + x31 * m.x13 + x32 * m.x23 + x33 * m.x33
      )
//      Matrix(
//        y00 = x00 * m.x00 + x10 * m.x01 + x20 * m.x02 + x30 * m.x03,
//        y01 = x00 * m.x10 + x10 * m.x11 + x20 * m.x12 + x30 * m.x13,
//        y02 = x00 * m.x20 + x10 * m.x21 + x20 * m.x22 + x30 * m.x23,
//        y03 = x00 * m.x30 + x10 * m.x31 + x20 * m.x32 + x30 * m.x33,
//
//        y10 = x01 * m.x00 + x11 * m.x01 + x21 * m.x02 + x31 * m.x03,
//        y11 = x01 * m.x10 + x11 * m.x11 + x21 * m.x12 + x31 * m.x13,
//        y12 = x01 * m.x20 + x11 * m.x21 + x21 * m.x22 + x31 * m.x23,
//        y13 = x01 * m.x30 + x11 * m.x31 + x21 * m.x32 + x31 * m.x33,
//
//        y20 = x02 * m.x00 + x12 * m.x01 + x22 * m.x02 + x32 * m.x03,
//        y21 = x02 * m.x10 + x12 * m.x11 + x22 * m.x12 + x32 * m.x13,
//        y22 = x02 * m.x20 + x12 * m.x21 + x22 * m.x22 + x32 * m.x23,
//        y23 = x02 * m.x30 + x12 * m.x31 + x22 * m.x32 + x32 * m.x33,
//
//        y30 = x03 * m.x00 + x13 * m.x01 + x23 * m.x02 + x33 * m.x03,
//        y31 = x03 * m.x10 + x13 * m.x11 + x23 * m.x12 + x33 * m.x13,
//        y32 = x03 * m.x20 + x13 * m.x21 + x23 * m.x22 + x33 * m.x23,
//        y33 = x03 * m.x30 + x13 * m.x31 + x23 * m.x32 + x33 * m.x33
//      )

    @targetName("add")
    infix def +(m: Matrix): Matrix =
      Matrix(
        y00 = x00 * m.x00,
        y01 = x01 * m.x01,
        y02 = x02 * m.x02,
        y03 = x03 * m.x03,

        y10 = x10 * m.x10,
        y11 = x11 * m.x11,
        y12 = x12 * m.x12,
        y13 = x13 * m.x13,

        y20 = x20 * m.x20,
        y21 = x21 * m.x21,
        y22 = x22 * m.x22,
        y23 = x23 * m.x23,

        y30 = x30 * m.x30,
        y31 = x31 * m.x31,
        y32 = x32 * m.x32,
        y33 = x33 * m.x33
      )

    def ×(v: HVector3D): HVector3D =
      HVector3D(
        x00 * v.x + x01 * v.y + x02 * v.z + x03 * v.w,
        x10 * v.x + x11 * v.y + x12 * v.z + x13 * v.w,
        x20 * v.x + x21 * v.y + x22 * v.z + x23 * v.w,
        x30 * v.x + x31 * v.y + x32 * v.z + x33 * v.w
      )


    lazy val transpose: Matrix =
      Matrix(
        y00 = x00,
        y01 = x10,
        y02 = x20,
        y03 = x30,

        y10 = x01,
        y11 = x11,
        y12 = x21,
        y13 = x31,

        y20 = x02,
        y21 = x12,
        y22 = x22,
        y23 = x32,

        y30 = x03,
        y31 = x13,
        y32 = x23,
        y33 = x33
      )
      
    lazy val determinant: T =
        x03*x12*x21*x30 - x02*x13*x21*x30 - x03*x11*x22*x30 + x01*x13*x22*x30+
        x02*x11*x23*x30 - x01*x12*x23*x30 - x03*x12*x20*x31 + x02*x13*x20*x31+
        x03*x10*x22*x31 - x00*x13*x22*x31 - x02*x10*x23*x31 + x00*x12*x23*x31+
        x03*x11*x20*x32 - x01*x13*x20*x32 - x03*x10*x21*x32 + x00*x13*x21*x32+
        x01*x10*x23*x32 - x00*x11*x23*x32 - x02*x11*x20*x33 + x01*x12*x20*x33+
        x02*x10*x21*x33 - x00*x12*x21*x33 - x01*x10*x22*x33 + x00*x11*x22*x33

    /**
     * Should be overriden when simplifications are possible
     */
    lazy val optInverse: Option[Matrix] =
      Matrix(
        y00 = x12 * x23 * x31 - x13 * x22 * x31 + x13 * x21 * x32 - x11 * x23 * x32 - x12 * x21 * x33 + x11 * x22 * x33,
        y01 = x03 * x22 * x31 - x02 * x23 * x31 - x03 * x21 * x32 + x01 * x23 * x32 + x02 * x21 * x33 - x01 * x22 * x33,
        y02 = x02 * x13 * x31 - x03 * x12 * x31 + x03 * x11 * x32 - x01 * x13 * x32 - x02 * x11 * x33 + x01 * x12 * x33,
        y03 = x03 * x12 * x21 - x02 * x13 * x21 - x03 * x11 * x22 + x01 * x13 * x22 + x02 * x11 * x23 - x01 * x12 * x23,
        y10 = x13 * x22 * x30 - x12 * x23 * x30 - x13 * x20 * x32 + x10 * x23 * x32 + x12 * x20 * x33 - x10 * x22 * x33,
        y11 = x02 * x23 * x30 - x03 * x22 * x30 + x03 * x20 * x32 - x00 * x23 * x32 - x02 * x20 * x33 + x00 * x22 * x33,
        y12 = x03 * x12 * x30 - x02 * x13 * x30 - x03 * x10 * x32 + x00 * x13 * x32 + x02 * x10 * x33 - x00 * x12 * x33,
        y13 = x02 * x13 * x20 - x03 * x12 * x20 + x03 * x10 * x22 - x00 * x13 * x22 - x02 * x10 * x23 + x00 * x12 * x23,
        y20 = x11 * x23 * x30 - x13 * x21 * x30 + x13 * x20 * x31 - x10 * x23 * x31 - x11 * x20 * x33 + x10 * x21 * x33,
        y21 = x03 * x21 * x30 - x01 * x23 * x30 - x03 * x20 * x31 + x00 * x23 * x31 + x01 * x20 * x33 - x00 * x21 * x33,
        y22 = x01 * x13 * x30 - x03 * x11 * x30 + x03 * x10 * x31 - x00 * x13 * x31 - x01 * x10 * x33 + x00 * x11 * x33,
        y23 = x03 * x11 * x20 - x01 * x13 * x20 - x03 * x10 * x21 + x00 * x13 * x21 + x01 * x10 * x23 - x00 * x11 * x23,
        y30 = x12 * x21 * x30 - x11 * x22 * x30 - x12 * x20 * x31 + x10 * x22 * x31 + x11 * x20 * x32 - x10 * x21 * x32,
        y31 = x01 * x22 * x30 - x02 * x21 * x30 + x02 * x20 * x31 - x00 * x22 * x31 - x01 * x20 * x32 + x00 * x21 * x32,
        y32 = x02 * x11 * x30 - x01 * x12 * x30 - x02 * x10 * x31 + x00 * x12 * x31 + x01 * x10 * x32 - x00 * x11 * x32,
        y33 = x01 * x12 * x20 - x02 * x11 * x20 + x02 * x10 * x21 - x00 * x12 * x21 - x01 * x10 * x22 + x00 * x11 * x22
      ) / determinant

    final infix def /(s:T): Option[Matrix] =
      if s == 0 then
        None
      else
       Some(
         Matrix(
          y00 = x00 / s,
          y01 = x10 / s,
          y02 = x20 / s,
          y03 = x30 / s,

          y10 = x01 / s,
          y11 = x11 / s,
          y12 = x21 / s,
          y13 = x31 / s,

          y20 = x02 / s,
          y21 = x12 / s,
          y22 = x22 / s,
          y23 = x32 / s,

          y30 = x03 / s,
          y31 = x13 / s,
          y32 = x23 / s,
          y33 = x33 / s
        )
       )
        
    final override def equals(obj: Any): Boolean =
      obj match
        case that: Matrix if (that != null) =>
          (this.x00 ~= that.x00) &&
            (this.x01 ~= that.x01) &&
            (this.x02 ~= that.x02) &&
            (this.x03 ~= that.x03) &&

          (this.x10 ~= that.x10) &&
            (this.x11 ~= that.x11) &&
            (this.x12 ~= that.x12) &&
            (this.x13 ~= that.x13) &&

          (this.x20 ~= that.x20) &&
            (this.x21 ~= that.x21) &&
            (this.x22 ~= that.x22) &&
            (this.x23 ~= that.x23) &&

          (this.x30 ~= that.x30) &&
            (this.x31 ~= that.x31) &&
            (this.x32 ~= that.x32) &&
            (this.x33 ~= that.x33)
        case _ =>
          false
    
    override def toString: String =
      s"""$x00, $x01, $x02, $x03
         |$x10, $x11, $x12, $x13
         |$x20, $x21, $x22, $x23
         |$x30, $x31, $x32, $x33""".stripMargin 

  object Matrix:
    def apply(y00: T, y01: T, y02: T, y03: T,
              y10: T, y11: T, y12: T, y13: T,
              y20: T, y21: T, y22: T, y23: T,
              y30: T, y31: T, y32: T, y33: T): Matrix =
      new Matrix {
        override val x00 = y00
        override val x01 = y01
        override val x02 = y02
        override val x03 = y03

        override val x10 = y10
        override val x11 = y11
        override val x12 = y12
        override val x13 = y13

        override val x20 = y20
        override val x21 = y21
        override val x22 = y22
        override val x23 = y23

        override val x30 = y30
        override val x31 = y31
        override val x32 = y32
        override val x33 = y33
      }

    def diagonal(x0: T,
                 x1: T,
                 x2: T,
                 x3: T) =
      Matrix(
        y00 = x0,
        y01 = _0,
        y02 = _0,
        y03 = _0,

        y10 = _0,
        y11 = x1,
        y12 = _0,
        y13 = _0,

        y20 = _0,
        y21 = _0,
        y22 = x2,
        y23 = _0,

        y30 = _0,
        y31 = _0,
        y32 = _0,
        y33 = x3
      )

    val Identity =
      diagonal(__1, __1, __1, __1)


  trait AffineTransformation extends Matrix :
    final override val x30: T = _0
    final override val x31: T = _0
    final override val x32: T = _0
    final override val x33: T = __1

    lazy val right: SpaceVector =
      SpaceVector(
        x00,
        x10,
        x20
      )

    lazy val up: SpaceVector =
      SpaceVector(
        x01,
        x11,
        x21
      )

    lazy val forward: SpaceVector =
      SpaceVector(
        x02,
        x12,
        x22
      )

    lazy val position: SpaceVector =
      SpaceVector(
        x03,
        x13,
        x23
      )

  case class Homothety(ratio: T)
    extends AffineTransformation :
    override val x00 = ratio
    override val x01 = _0
    override val x02 = _0
    override val x03 = _0

    override val x10 = _0
    override val x11 = ratio
    override val x12 = _0
    override val x13 = _0

    override val x20 = _0
    override val x21 = _0
    override val x22 = ratio
    override val x23 = _0

    override lazy val optInverse: Option[Homothety] =
      if ratio == _0 then
        None
      else
        Some(Homothety(_1 / ratio))

  case class Scaling3D(ratio: SpaceVector)
    extends AffineTransformation :
    override val x00 = ratio.x
    override val x01 = _0
    override val x02 = _0
    override val x03 = _0

    override val x10 = _0
    override val x11 = ratio.y
    override val x12 = _0
    override val x13 = _0

    override val x20 = _0
    override val x21 = _0
    override val x22 = ratio.z
    override val x23 = _0

    override lazy val optInverse: Option[Scaling3D] =
      ratio.inverse.map(Scaling3D(_))

  object Scaling3D:
    def isotropic(ratio: T): Scaling3D =
      Scaling3D(SpaceVector.fill(ratio))


  case class Translation3D(vector: SpaceVector)
    extends AffineTransformation :
    override val x00 = __1
    override val x01 = _0
    override val x02 = _0
    override val x03 = vector.x

    override val x10 = _0
    override val x11 = __1
    override val x12 = _0
    override val x13 = vector.y

    override val x20 = _0
    override val x21 = _0
    override val x22 = __1
    override val x23 = vector.z

    override lazy val optInverse: Option[Translation3D] =
      Some(inverse)

    lazy val inverse: Translation3D =
      Translation3D(-vector)


  case class StandardPerspectiveProjection private(planeDistance: T)
    extends Matrix :
    override val x00 = __1
    override val x01 = _0
    override val x02 = _0
    override val x03 = _0

    override val x10 = _0
    override val x11 = __1
    override val x12 = _0
    override val x13 = _0

    override val x20 = _0
    override val x21 = _0
    override val x22 = _0
    override val x23 = _0

    override val x30 = _0
    override val x31 = _0
    override val x32 = __1 / planeDistance
    override val x33 = _1

  object StandardPerspectiveProjection:
    def safe(planeDistance: T): Option[StandardPerspectiveProjection] =
      if planeDistance == _0 then
        None
      else
        Some(StandardPerspectiveProjection(planeDistance))

  trait AffineRotation
    extends AffineTransformation:
    override val x03 = _0
    override val x13 = _0
    override val x23 = _0


  case class BasisTransformation(basis: Basis)
    extends AffineRotation:
    override val x00 = basis.i.x
    override val x01 = basis.j.x
    override val x02 = basis.k.x

    override val x10 = basis.i.y
    override val x11 = basis.j.y
    override val x12 = basis.k.y

    override val x20 = basis.i.z
    override val x21 = basis.j.z
    override val x22 = basis.k.z

    override lazy val optInverse: Option[Matrix] =
      Some(inverse)

    lazy val inverse: Matrix =
      transpose

  case class CoordinateSystemTransformation(origin: SpacePoint,
                                            basis: Basis)
    extends AffineTransformation:
    override val x00 = basis.i.x
    override val x01 = basis.j.x
    override val x02 = basis.k.x
    override val x03 = origin.x

    override val x10 = basis.i.y
    override val x11 = basis.j.y
    override val x12 = basis.k.y
    override val x13 = origin.y

    override val x20 = basis.i.z
    override val x21 = basis.j.z
    override val x22 = basis.k.z
    override val x23 = origin.z

    override lazy val optInverse: Option[Matrix] =
      Some(inverse)

    lazy val inverse: Matrix =
      new AffineTransformation:
        override val x00 = basis.i.x
        override val x01 = basis.i.y
        override val x02 = basis.i.z
        override val x03 = -origin.x

        override val x10 = basis.j.x
        override val x11 = basis.j.y
        override val x12 = basis.j.z
        override val x13 = -origin.y

        override val x20 = basis.k.x
        override val x21 = basis.k.y
        override val x22 = basis.k.z
        override val x23 = -origin.z


  trait AxisRotation
    extends AffineRotation:
    val angle: Radians

  object AxisRotation:
    def apply(q: Quaternion): AxisRotation =
      val w = q.a
      val x = q.b
      val y = q.c
      val z = q.d
      new AxisRotation :
        override val angle: Radians = q.rotationAngle

        override val x00 = __1 - __2 * y * y - __2 * z * z
        override val x01 = __2 * x * y - __2 * w * z
        override val x02 = __2 * x * z + __2 * w * y

        override val x10 = __2 * x * y + __2 * w * z
        override val x11 = __1 - __2 * x * x - __2 * z * z
        override val x12 = __2 * y * z - __2 * w * x

        override val x20 = __2 * x * z - __2 * w * y
        override val x21 = __2 * y * z - __2 * w * x
        override val x22 = __1 - __2 * x * x - __2 * y * y


  trait BaseAxisRotation
    extends AxisRotation :
    val axis: Axis

  case class OxRotation(angle: Radians)
    extends BaseAxisRotation :
    override val axis: Axis = Axis.X

    override val x00 = __1
    override val x01 = _0
    override val x02 = _0

    override val x10 = _0
    override val x11 = cos(angle)
    override val x12 = sin(angle)

    override val x20 = _0
    override val x21 = -sin(angle)
    override val x22 = cos(angle)

  case class OyRotation(angle: Radians)
    extends BaseAxisRotation :
    override val axis: Axis = Axis.Y

    override val x00 = cos(angle)
    override val x01 = _0
    override val x02 = -sin(angle)

    override val x10 = _0
    override val x11 = __1
    override val x12 = _0

    override val x20 = sin(angle)
    override val x21 = _0
    override val x22 = cos(angle)

  case class OzRotation(angle: Radians)
    extends BaseAxisRotation :
    override val axis: Axis = Axis.Z

    override val x00 = cos(angle)
    override val x01 = sin(angle)
    override val x02 = _0

    override val x10 = -sin(angle)
    override val x11 = cos(angle)
    override val x12 = _0

    override val x20 = _0
    override val x21 = _0
    override val x22 = __1

  