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

/**
 * @since 0.2.0
 * @author gweinbach on 14/06/2022
 */
trait TransformationMatrix[T: Numeric : Trig : Precision]
  extends Space[T] :

  private val _num = Numeric[T]
  private val _0: T = _num.zero
  private val _1: T = _num.one

  case class HVector(x: T,
                     y: T,
                     z: T,
                     w: T):

    lazy val cartesian: Option[Vector] =
      if isAtInfinity then
        None
      else
        Some(Vector(x / w, y / w, z / w))

    lazy val isAtInfinity: Boolean =
      w == 0

    def x(m: Matrix): HVector =
      HVector(
        x * m.x00 + y * m.x10 + z * m.x20 + w * m.x30,
        x * m.x01 + y * m.x11 + z * m.x21 + w * m.x31,
        x * m.x02 + y * m.x12 + z * m.x22 + w * m.x32,
        x * m.x03 + y * m.x13 + z * m.x23 + w * m.x33
      )

    override def equals(obj: Any): Boolean =
      obj match
        case that: HVector if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y) &&
            (this.z ~= that.z) &&
            (this.w ~= that.w)
        case _ =>
          false

  object HVector:
    def apply(v: Vector): HVector =
      HVector(v.x, v.y, v.z, _1)

    val Zero = HVector(_0, _0, _0, _0)
    val One = HVector(_1, _1, _1, _1)

  extension (v: Vector)
    def homogeneous: HVector =
      HVector(v)

  trait Matrix:
    val x00, x01, x02, x03: T
    val x10, x11, x12, x13: T
    val x20, x21, x22, x23: T
    val x30, x31, x32, x33: T

    def x(m: Matrix): Matrix =
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

  trait AffineTransformation extends Matrix :
    final override val x03: T = _0
    final override val x13: T = _0
    final override val x23: T = _0
    final override val x33: T = _1

  case class Homothety(ratio: Vector)
    extends AffineTransformation:
    override val x00 = ratio.x
    override val x01 = _0
    override val x02 = _0

    override val x10 = _0
    override val x11 = ratio.y
    override val x12 = _0

    override val x20 = _0
    override val x21 = _0
    override val x22 = ratio.z

    override val x30 = _0
    override val x31 = _0
    override val x32 = _0

    def inverse: Option[Homothety] =
      ratio.inverse.map(Homothety(_))


  object Homothety:
    def global(ratio: T): Homothety =
      Homothety(Vector.fill(ratio))

  case class AffineTranslation(vector: Vector)
    extends AffineTransformation
//    override val x00 = _1
//    override val x01 = _0
//    override val x02 = _0
//
//    override val x10 = _0
//    override val x11 = _1
//    override val x12 = _0
//
//    override val x20 = _0
//    override val x21 = _0
//    override val x22 = _1
//
//    override val x30 = vector.x
//    override val x31 = vector.y
//    override val x32 = vector.z
