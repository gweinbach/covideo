/*
 * @author gweinbach on 15/06/2022 22:46
 * @since 0.2.0
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.*
import spire.*
import spire.implicits.*
import spire.math.*

/**
 * @since 0.2.0
 * @author gweinbach on 15/06/2022
 */
trait Plane[T: Numeric : Precision]:

  private val _Numeric = summon[Numeric[T]]

  private val _0: T = _Numeric.zero
  private val __1: T = _Numeric.one // double '_' to avoid conflict with Product<X>._1

  type Object2D = PlanePoint | PlaneVector

  case class PlanePoint(x: T,
                        y: T):

    override def equals(obj: Any): Boolean =
      obj match
        case that: PlanePoint if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y)
        case _ =>
          false

  object PlanePoint:

    val Origin: PlanePoint = PlanePoint(_0, _0)
    val OneX: PlanePoint = PlanePoint(__1, _0)
    val OneY: PlanePoint = PlanePoint(_0, __1)

  sealed trait PlaneVector:
    val x: T
    val y: T

    final lazy val tuple: (T, T) =
      (x, y)

    final lazy val isNull: Boolean =
      PlaneVector.isNullTuple(x, y)

    final lazy val nonNull: Option[NonNullPlaneVector] =
      PlaneVector.nonNull(this)

    final lazy val magnitude: T =
      _Numeric.sqrt(this ⋅ this)

    final lazy val isNormalized: Boolean =
      magnitude ~= __1

    final def dest(origin: PlanePoint = PlanePoint.Origin): PlanePoint =
      PlanePoint(origin.x + x, origin.y + y)

    def unary_- : PlaneVector

    def isColinear(v: PlaneVector): Boolean

    infix def isOrthogonal(v: PlaneVector): Boolean =
      this ⋅ v ~= _0

    lazy val inverse: Option[PlaneVector]

    infix def +(v: PlaneVector): PlaneVector =
      PlaneVector(x + v.x, y + v.y)

    infix def -(v: PlaneVector): PlaneVector =
      PlaneVector(x - v.x, y - v.y)

    infix def *(n: T): PlaneVector

    protected def divideBy(n: T): PlaneVector

    infix def /(n: T): Option[PlaneVector]

    infix def ⋅(v: PlaneVector): T =
      x * v.x + y * v.y

    override def equals(obj: Any): Boolean =
      obj match
        case that: PlaneVector if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y)
        case _ =>
          false


  case object NullPlaneVector
    extends PlaneVector :
    override val x: T = _0
    override val y: T = _0

    override def unary_- = NullPlaneVector

    override infix def isColinear(v: PlaneVector): Boolean =
      true

    override infix def isOrthogonal(v: PlaneVector): Boolean =
      true

    override lazy val inverse: None.type =
      None

    override def +(v: PlaneVector): PlaneVector =
      v match
        case NullPlaneVector =>
          NullPlaneVector
        case _ =>
          v

    override def -(v: PlaneVector): PlaneVector =
      v match
        case NullPlaneVector =>
          NullPlaneVector
        case _ =>
          -v

    override def *(n: T): NullPlaneVector.type =
      NullPlaneVector

    override protected def divideBy(n: T): NullPlaneVector.type =
      NullPlaneVector

    override def /(n: T): Option[NullPlaneVector.type] =
      if n ~= _0 then
        None
      else
        Some(NullPlaneVector)

    override infix def ⋅(v: PlaneVector): T =
      _0


  case class NonNullPlaneVector private(x: T,
                                        y: T)
    extends PlaneVector:

    lazy val normalized: NonNullPlaneVector =
      divideBy(magnitude)

    override def unary_- : NonNullPlaneVector =
      NonNullPlaneVector(-x, -y)

    override def isColinear(v: PlaneVector): Boolean =
      x * v.y ~= y * v.x

    override lazy val inverse: Option[NonNullPlaneVector] =
      if (x ~= _0) || (y ~= _0) then
        None
      else
        Some(NonNullPlaneVector(__1 / x, __1 / y))

    final lazy val orthogonalDirect: NonNullPlaneVector =
      NonNullPlaneVector(-y, x)

    override def *(n: T): PlaneVector =
      if n ~= _0 then
        NullPlaneVector
      else
        NonNullPlaneVector(
          x * n,
          y * n
        )

    override protected def divideBy(n: T): NonNullPlaneVector =
      NonNullPlaneVector(
        x / n,
        y / n
      )

    override final def /(n: T): Option[NonNullPlaneVector] =
      if n ~= _0 then
        None
      else
        Some(
          divideBy(n)
        )


  object NonNullPlaneVector:
    val OneX = NonNullPlaneVector(__1, _0)
    val OneY = NonNullPlaneVector(_0, __1)

    inline def isNullTuple(x: T, y: T): Boolean =
      (x ~= _0) && (y ~= _0)

    inline def safe(x: T,
                    y: T): Option[NonNullPlaneVector] =
      if isNullTuple(x, y) then
        None
      else
        Some(NonNullPlaneVector(x, y))

//    def nonNull(magnitude: T,
//                axis: Axis): Option[NonNullPlaneVector] =
//      if magnitude ~= _0 then
//        None
//      else
//        Some(
//          axis match
//            case Axis.X => NonNullPlaneVector(magnitude, _0)
//            case Axis.Y => NonNullPlaneVector(_0, magnitude)
//        )


  object PlaneVector:

    inline def isNullTuple(x: T, y: T): Boolean =
      NonNullPlaneVector.isNullTuple(x, y)

    inline def apply(x: T, y: T): PlaneVector =
      nonNull(x, y).fold(NullPlaneVector)(v => v)

    inline def apply(start: PlanePoint,
                     end: PlanePoint): PlaneVector =
      nonNull(start, end).fold(NullPlaneVector)(v => v)

    inline def apply(end: PlanePoint): PlaneVector =
      nonNull(end).fold(NullPlaneVector)(v => v)

//    inline def apply(magnitude: T,
//                     axis: Axis): PlaneVector =
//      nonNull(magnitude, axis).fold(NullPlaneVector)(v => v)

    def nonNull(x: T,
                y: T): Option[NonNullPlaneVector] =
      NonNullPlaneVector.safe(x, y)

    def nonNull(start: PlanePoint,
                end: PlanePoint): Option[NonNullPlaneVector] =
      nonNull(end.x - start.x, end.y - start.y)

    def nonNull(end: PlanePoint): Option[NonNullPlaneVector] =
      nonNull(PlanePoint.Origin, end)

    def nonNull(v: PlaneVector): Option[NonNullPlaneVector] =
      nonNull(v.x, v.y)

//    def nonNull(magnitude: T,
//                axis: Axis): Option[NonNullPlaneVector] =
//      NonNullPlaneVector.nonNull(magnitude, axis)

//    def nonNullCrossProduct(v1: PlaneVector,
//                            v2: PlaneVector): Option[NonNullPlaneVector] =
//      val nx = v1.y * v2.z - v1.z * v2.y
//      val ny = v1.z * v2.x - v1.x * v2.z
//      val nz = v1.x * v2.y - v1.y * v2.x
//      PlaneVector.nonNull(nx, ny, nz)

    def fill(t: T): PlaneVector =
      PlaneVector(t, t)

    val Null = NullPlaneVector

    val OneX = NonNullPlaneVector.OneX
    val OneY = NonNullPlaneVector.OneY

//  trait PlaneVector(x: T,
//                     y: T):
//
//    inline def unary_- =
//      PlaneVector(-x, -y)
//
//    lazy val inverse: Option[PlaneVector] =
//      if (x ~= _0) || (y ~= _0) then
//        None
//      else
//        Some(PlaneVector(__1 / x, __1 / y))
//
//    override def equals(obj: Any): Boolean =
//      obj match
//        case that: PlaneVector if (that != null) =>
//          (this.x ~= that.x) &&
//            (this.y ~= that.y)
//        case _ =>
//          false
//
//  object PlaneVector:
//
//    val Null: PlaneVector = PlaneVector(_0, _0)
//    val OneX: PlaneVector = PlaneVector(__1, _0)
//    val OneY: PlaneVector = PlaneVector(_0, __1)
//
//    def apply(p1: PlanePoint,
//              p2: PlanePoint): PlaneVector =
//      new PlaneVector(
//        p2.x - p1.x,
//        p2.y - p1.y
//      )
//
//    def apply(p2: PlanePoint): PlaneVector =
//      apply(PlanePoint.Origin, p2)
//
//    def fill(t: T): PlaneVector =
//      PlaneVector(t, t)


  case class PlaneVertex(s: PlanePoint,
                         t: PlanePoint)

  case class ClippingWindow(width: T,
                            height: T):
    override def equals(obj: Any): Boolean =
      obj match
        case that: ClippingWindow if (that != null) =>
          (this.width ~= that.width) &&
            (this.height ~= that.height)
        case _ =>
          false


  sealed trait Basis2D:
    self =>

    lazy val i: NonNullPlaneVector
    lazy val j: NonNullPlaneVector

    lazy val normalized: NormalizedBasis2D =
      new NormalizedBasis2D :
        override lazy val i: NonNullPlaneVector = self.i.normalized
        override lazy val j: NonNullPlaneVector = self.j.normalized

    lazy val isNormalized: Boolean =
      Basis2D.isNormalized(i, j)

    lazy val isOrthogonal: Boolean =
      Basis2D.isOrthogonal(i, j)

    override def equals(obj: Any): Boolean =
      obj match
        case that: Basis2D if (that != null) =>
          (this.i == that.i) &&
            (this.j == that.j)
        case _ =>
          false

    override def hashCode(): Int =
      31 * (
        i.##
        ) + j.##

    override def toString: String =
      s"Basis2D($i, $j)"

  trait NormalizedBasis2D extends Basis2D :
    override lazy val normalized: NormalizedBasis2D =
      this

  trait OrthogonalBasis2D extends Basis2D :
    self =>
    override lazy val normalized: OrthonormalBasis2D =
      new OrthonormalBasis2D :
        override lazy val i: NonNullPlaneVector = self.i.normalized
        override lazy val j: NonNullPlaneVector = self.j.normalized

  trait OrthonormalBasis2D extends OrthogonalBasis2D with NormalizedBasis2D:
    override lazy val normalized: OrthonormalBasis2D =
      this

  object Basis2D:

    def isOrthogonal(i: NonNullPlaneVector,
                     j: NonNullPlaneVector): Boolean =
      (i isOrthogonal j)

    def isNormalized(i: NonNullPlaneVector,
                     j: NonNullPlaneVector): Boolean =
      i.isNormalized && j.isNormalized

    def safe(i: PlaneVector,
             j: PlaneVector): Option[Basis2D] =
      for
        baseI <- PlaneVector.nonNull(i)
        baseJ <- PlaneVector.nonNull(j) if !(baseI isColinear baseJ)
      yield
        new Basis2D :
          override lazy val i: NonNullPlaneVector = baseI
          override lazy val j: NonNullPlaneVector = baseJ

    def orthogonal(i: PlaneVector,
                   j: PlaneVector): Option[OrthogonalBasis2D] =
      for
        baseI <- PlaneVector.nonNull(i)
        baseJ <- PlaneVector.nonNull(j) if !(baseI isColinear baseJ) && (baseI isOrthogonal baseJ)
      yield
        new OrthogonalBasis2D :
          override lazy val i: NonNullPlaneVector = baseI
          override lazy val j: NonNullPlaneVector = baseJ

    def orthogonal(baseI: NonNullPlaneVector): OrthogonalBasis2D =
        new OrthogonalBasis2D :
          override lazy val i: NonNullPlaneVector = baseI
          override lazy val j: NonNullPlaneVector = baseI.orthogonalDirect

    def normalized(i: PlaneVector,
                   j: PlaneVector): Option[NormalizedBasis2D] =
      safe(i, j).map(_.normalized)

    def orthonormal(i: PlaneVector,
                    j: PlaneVector): Option[OrthonormalBasis2D] =
      orthogonal(i, j).map(_.normalized)

    def orthonormal(i: NonNullPlaneVector): OrthonormalBasis2D =
      orthogonal(i).normalized

    val Normal: OrthonormalBasis2D =
      new OrthonormalBasis2D :
        override lazy val i: NonNullPlaneVector = NonNullPlaneVector.OneX
        override lazy val j: NonNullPlaneVector = NonNullPlaneVector.OneY
