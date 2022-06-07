/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.ez3d

import spire.*
import spire.algebra.Trig
import spire.implicits.*
import spire.math.*

/**
 * @since 0.2.0
 * @author gweinbach on 01/06/2022
 */
trait H[T: Fractional](spatialPrecision: T)
  extends Vectors[T]:

  private val frac = summon[Fractional[T]]

  private val _0 = frac.zero
  private val _1 = frac.one

  case class Quaternion(a: T,
                        b: T,
                        c: T,
                        d: T):

    infix def *(m: T): Quaternion = Quaternion(a * m, b * m, c * m, m * m)

    private def divideBy(d: T): Quaternion = Quaternion(a / d, b / d, c / d, d / d)

    infix def /(d: T): Option[Quaternion] =
      if d == _0 then
        None
      else
        Some(divideBy(d))

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
    lazy val imaginary: Vector =
      Vector(b, c, d)

    lazy val conjugate: Quaternion = Quaternion(a, b = -b, c = -c, d = -d)
    lazy val squareMagnitude: T = a * a + b * b + c * c + d * d
    lazy val magnitude: T = frac.sqrt(squareMagnitude)
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

  object Quaternion:
    def apply(real: T, imaginary: Vector): Quaternion =
      new Quaternion(real, imaginary.x, imaginary.y, imaginary.z)

    def fromRotationVector(axis: Vector,
                           angle: T)
                          (using ev: Trig[T]): Option[Quaternion] =
      val sinA = sin(angle / 2)
      val cosA = cos(angle / 2)

      Quaternion(
        cosA,
        axis * sinA
      ).normalized


  val Zero = Quaternion(_0, _0, _0, _0)
  val i = Quaternion(_0, _1, _0, _0)
  val j = Quaternion(_0, _0, _1, _0)
  val k = Quaternion(_0, _0, _0, _1)

//  extension (t1: (T, T, T))
//
//    infix def ⋅(t2: (T, T, T)): T =
//      t1._1 * t2._1 + t1._2 * t2._2 + t1._3 * t2._3
//
//    infix def ∧(t2: (T, T, T)): (T, T, T) =
//      (
//        t1._2 * t2._3 - t1._3 * t2._2,
//        t1._3 * t2._1 - t1._1 * t2._3,
//        t1._1 * t2._2 - t1._2 * t2._1
//      )
//
//    def magnitude: T =
//      frac.sqrt(t1._1 * t1._1 + t1._2 * t1._2 + t1._3 * t1._3)
//
//  extension (t: T)
//    private infix def dot(t2: (T, T, T)): (T, T, T) =
//      (t * t2._1, t * t2._2, t * t2._3)
//
