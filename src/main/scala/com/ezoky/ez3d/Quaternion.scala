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

/**
 * @since 0.2.0
 * @author gweinbach on 01/06/2022
 */
trait H[T: Numeric : Precision]
  extends Vectors[T] :

  private val _Numeric = summon[Numeric[T]]

  private val _0 = _Numeric.zero
  private val _1 = _Numeric.one

  case class Quaternion(a: T,
                        b: T,
                        c: T,
                        d: T):

    infix def *(x: T): Quaternion = Quaternion(a * x, b * x, c * x, d * x)

    private def divideBy(y: T): Quaternion = Quaternion(a / y, b / y, c / y, d / y)

    infix def /(y: T): Option[Quaternion] =
      if y == _0 then
        None
      else
        Some(divideBy(y))

    infix def x(q2: Quaternion): Option[Quaternion] =
      val v1 = Vector(b, c, d)
      val w1 = a
      val v2 = Vector(q2.b, q2.c, q2.d)
      val w2 = q2.a
      Quaternion(
        (w1 * w2) - (v1 ⋅ v2),
        (v2 * w1) + (v1 * w2) + (v1 ∧ v2)
      ).normalized

    lazy val real: T = a
    lazy val imaginary: Vector = Vector(b, c, d)

    lazy val conjugate: Quaternion = Quaternion(a, b = -b, c = -c, d = -d)
    lazy val squareMagnitude: T = a * a + b * b + c * c + d * d
    lazy val magnitude: T = _Numeric.sqrt(squareMagnitude)
    lazy val normalized: Option[Quaternion] = this / magnitude
    lazy val inverse: Option[Quaternion] = conjugate / squareMagnitude

    def rotationAngle(using trig: Trig[T]): T =
      acos(a) / 2

    def rotationAxis: Option[Vector] =
      Vector(b, c, d).normalized

    def rotate(vector: Vector): Option[Vector] =
      for
        q1 <- this x Quaternion(_0, vector)
        q2 <- q1 x conjugate
      yield q2.imaginary

    override def equals(obj: Any): Boolean =
      obj match
        case that: Quaternion if (that != null) =>
          (this.a ~= that.a) &&
            (this.b ~= that.b) &&
            (this.c ~= that.c) &&
            (this.d ~= that.d)
        case _ =>
          false


  object Quaternion:
    def apply(real: T, imaginary: Vector): Quaternion =
      new Quaternion(real, imaginary.x, imaginary.y, imaginary.z)

    def fromRotationVector(axis: Vector,
                           angle: T)
                          (using Trig[T]): Option[Quaternion] =
      val sinA = sin(angle / 2)
      val cosA = cos(angle / 2)

      Quaternion(
        cosA,
        axis * sinA
      ).normalized

    val Zero = Quaternion(_0, _0, _0, _0)
    val One = Quaternion(_1, _0, _0, _0)
    val I = Quaternion(_0, _1, _0, _0)
    val J = Quaternion(_0, _0, _1, _0)
    val K = Quaternion(_0, _0, _0, _1)
