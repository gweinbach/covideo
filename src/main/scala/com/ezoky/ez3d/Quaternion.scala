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

import scala.annotation.targetName

/**
 * @since 0.2.0
 * @author gweinbach on 01/06/2022
 */
trait H[T: Numeric : Precision]
  extends Space[T] with Angles[T]:

  private val _Numeric = summon[Numeric[T]]

  private val _0 = _Numeric.zero
  private val __1 = _Numeric.one // double '_' to avoid conflict with Product<X>._1

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

    @targetName("times")
    infix def ×(q2: Quaternion): Quaternion =
      val v1 = SpaceVector(b, c, d)
      val w1 = a
      val v2 = SpaceVector(q2.b, q2.c, q2.d)
      val w2 = q2.a
      Quaternion(
        (w1 * w2) - (v1 ⋅ v2),
        (v2 * w1) + (v1 * w2) + (v1 ∧ v2)
      )

    lazy val real: T = a
    lazy val imaginary: SpaceVector = SpaceVector(b, c, d)

    lazy val conjugate: Quaternion = Quaternion(a, b = -b, c = -c, d = -d)
    lazy val squareMagnitude: T = a * a + b * b + c * c + d * d
    lazy val magnitude: T = _Numeric.sqrt(squareMagnitude)
    lazy val normalized: Option[Quaternion] = this / magnitude
    lazy val inverse: Option[Quaternion] = conjugate / squareMagnitude

    def rotationAngle(using trig: Trig[T]): Radians =
      acos[Radians](a) / 2

    def rotationAxis: Option[NonNullSpaceVector] =
      SpaceVector.nonNull(b, c, d).map(_.normalized)

    def rotateNull(vector: NullSpaceVector.type ): NullSpaceVector.type =
      NullSpaceVector

    def rotateNonNull(vector: NonNullSpaceVector): NonNullSpaceVector =
      (this × Quaternion(_0, vector) × conjugate).imaginary.asInstanceOf[NonNullSpaceVector] // TODO: don't cast

    def rotate(vector: SpaceVector): SpaceVector =
      vector match
        case nonNullSpaceVector: NonNullSpaceVector =>
          rotateNonNull(nonNullSpaceVector)
        case nullSpaceVector: NullSpaceVector.type =>
          rotateNull(nullSpaceVector)

    def rotate(basis: Basis): Option[Basis] =
      for
        rotatedBasis <- basis.same(
          rotateNonNull(basis.i),
          rotateNonNull(basis.j),
          rotateNonNull(basis.k)
        )
      yield rotatedBasis

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
    def apply(real: T,
              imaginary: SpaceVector): Quaternion =
      new Quaternion(real, imaginary.x, imaginary.y, imaginary.z)

    def fromRotationVectorAndAngle(axis: NonNullSpaceVector,
                                   angle: Radians)
                                  (using Trig[T]): Quaternion =
      val sinA = sin(angle / 2)
      val cosA = cos(angle / 2)

      Quaternion(
        cosA,
        axis * sinA
      ).normalized.get // if axis is not null quaternion cannot be null

    /**
     * The rotation angle is the magnitude of the rotation axis
     */
    def fromRotationVector(axis: NonNullSpaceVector)
                          (using Trig[T]): Quaternion =

      val angle = (axis.magnitude radians)
      fromRotationVectorAndAngle(axis.normalized, angle)


    val Zero = Quaternion(_0, _0, _0, _0)
    val One = Quaternion(__1, _0, _0, _0)
    val I = Quaternion(_0, __1, _0, _0)
    val J = Quaternion(_0, _0, __1, _0)
    val K = Quaternion(_0, _0, _0, __1)
