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
trait Space[T: Numeric : Precision]:

  private val _SpatialNumeric: Numeric[T] = summon[Numeric[T]]

  private val _0: T = _SpatialNumeric.zero
  private val __1: T = _SpatialNumeric.one // double '_' to avoid conflict with Product<X>._1

  type Object3D = SpacePoint|SpaceVector

  sealed trait Axis:
    def base: NonNullSpaceVector

  object Axis:

    case object X extends Axis :
      override def base: NonNullSpaceVector = SpaceVector.OneX

    case object Y extends Axis :
      override def base: NonNullSpaceVector = SpaceVector.OneY

    case object Z extends Axis :
      override def base: NonNullSpaceVector = SpaceVector.OneZ

  case class SpacePoint(x: T,
                        y: T,
                        z: T):

    infix def +(v: SpaceVector): SpacePoint =
      SpacePoint(x + v.x, y + v.y, z + v.z)

    def withX(x: T): SpacePoint =
      copy(x = x)

    def withY(y: T): SpacePoint =
      copy(y = y)

    def withZ(z: T): SpacePoint =
      copy(z = z)

    override def equals(obj: Any): Boolean =
      obj match
        case that: SpacePoint if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y) &&
            (this.z ~= that.z)
        case _ =>
          false


  object SpacePoint:
    val Origin = SpacePoint(_0, _0, _0)

    val OneX = SpacePoint(__1, _0, _0)
    val OneY = SpacePoint(_0, __1, _0)
    val OneZ = SpacePoint(_0, _0, __1)


  sealed trait SpaceVector:
    val x: T
    val y: T
    val z: T

    final lazy val tuple: (T, T, T) =
      (x, y, z)

    final lazy val isNull: Boolean =
      SpaceVector.isNullTuple(x, y, z)

    final lazy val magnitude: T =
      _SpatialNumeric.sqrt(this ⋅ this)

    final lazy val isNormalized: Boolean =
      magnitude == __1

    final def dest(origin: SpacePoint = SpacePoint.Origin): SpacePoint =
      SpacePoint(origin.x + x, origin.y + y, origin.z + z)

    def unary_- : SpaceVector

    def isCollinear(v: SpaceVector): Boolean

    lazy val inverse: Option[SpaceVector]

    infix def +(v: SpaceVector): SpaceVector =
      SpaceVector(x + v.x, y + v.y, z + v.z)

    infix def -(v: SpaceVector): SpaceVector =
      SpaceVector(x - v.x, y - v.y, z - v.z)

    infix def *(n: T): SpaceVector

    protected def divideBy(n: T): SpaceVector

    infix def /(n: T): Option[SpaceVector]

    infix def ⋅(v: SpaceVector): T =
      x * v.x + y * v.y + z * v.z

    infix def ∧(v: SpaceVector): SpaceVector

    override def equals(obj: Any): Boolean =
      obj match
        case that: SpaceVector if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y) &&
            (this.z ~= that.z)
        case _ =>
          false


  case object NullSpaceVector
    extends SpaceVector :
    override val x: T = _0
    override val y: T = _0
    override val z: T = _0

    override def unary_- = NullSpaceVector

    override def isCollinear(v: SpaceVector): Boolean =
      true

    override lazy val inverse: None.type =
      None

    override def +(v: SpaceVector): SpaceVector =
      v match
        case NullSpaceVector =>
          NullSpaceVector
        case _ =>
          v

    override def -(v: SpaceVector): SpaceVector =
      v match
        case NullSpaceVector =>
          NullSpaceVector
        case _ =>
          -v

    override def *(n: T): NullSpaceVector.type =
      NullSpaceVector

    override protected def divideBy(n: T): NullSpaceVector.type =
      NullSpaceVector

    override def /(n: T): Option[NullSpaceVector.type] =
      if n == _0 then
        None
      else
        Some(NullSpaceVector)

    override infix def ⋅(v: SpaceVector): T =
      _0

    override infix def ∧(v: SpaceVector): NullSpaceVector.type =
      NullSpaceVector

  case class NonNullSpaceVector(x: T,
                                y: T,
                                z: T)
    extends SpaceVector:

    lazy val normalized: NonNullSpaceVector =
      divideBy(magnitude)

    override def unary_- : NonNullSpaceVector =
      NonNullSpaceVector(-x, -y, -z)

    override def isCollinear(v: SpaceVector): Boolean =
      (this ∧ v).isNull

    override lazy val inverse: Option[NonNullSpaceVector] =
      if (x ~= _0) || (y ~= _0) || (z ~= _0) then
        None
      else
        Some(NonNullSpaceVector(__1 / x, __1 / y, __1 / z))

    override def *(n: T): SpaceVector =
      if n == _0 then
        NullSpaceVector
      else
        NonNullSpaceVector(
          x * n,
          y * n,
          z * n
        )

    override protected def divideBy(n: T): NonNullSpaceVector =
      NonNullSpaceVector(
        x / n,
        y / n,
        z / n
      )

    override final def /(n: T): Option[NonNullSpaceVector] =
      if n == _0 then
        None
      else
        Some(
          divideBy(n)
        )

    override final infix def ∧(v: SpaceVector): SpaceVector =
      SpaceVector.nonNullCrossProduct(this, v).fold(NullSpaceVector)(v => v)

  object NonNullSpaceVector:
    val OneX = NonNullSpaceVector(__1, _0, _0)
    val OneY = NonNullSpaceVector(_0, __1, _0)
    val OneZ = NonNullSpaceVector(_0, _0, __1)

  object SpaceVector:

    def isNullTuple(x: T, y: T, z: T): Boolean =
      (x ~= _0) && (y ~= _0) && (z ~= _0)

    inline def apply(x: T, y: T, z: T): SpaceVector =
      nonNull(x, y, z).fold(NullSpaceVector)(v => v)

    inline def apply(start: SpacePoint,
                     end: SpacePoint): SpaceVector =
      nonNull(start, end).fold(NullSpaceVector)(v => v)

    inline def apply(end: SpacePoint): SpaceVector =
      nonNull(end).fold(NullSpaceVector)(v => v)

    inline def apply(magnitude: T,
                     axis: Axis): SpaceVector =
      nonNull(magnitude, axis).fold(NullSpaceVector)(v => v)

    def nonNull(x: T,
                       y: T,
                       z: T): Option[NonNullSpaceVector] =
      if isNullTuple(x, y, z) then
        None
      else
        Some(NonNullSpaceVector(x, y, z))

    def nonNull(start: SpacePoint,
                end: SpacePoint): Option[NonNullSpaceVector] =
      if start == end then
        None
      else
        Some(NonNullSpaceVector(end.x - start.x, end.y - start.y, end.z - start.z))

    def nonNull(end: SpacePoint): Option[NonNullSpaceVector] =
      nonNull(SpacePoint.Origin, end)

    def nonNull(v: SpaceVector): Option[NonNullSpaceVector] =
      if v.isNull then
        None
      else
        Some(NonNullSpaceVector(v.x, v.y, v.z))

    def nonNull(magnitude: T,
                       axis: Axis): Option[NonNullSpaceVector] =
      if magnitude == _0 then
        None
      else
        Some(
          axis match
            case Axis.X => NonNullSpaceVector(magnitude, _0, _0)
            case Axis.Y => NonNullSpaceVector(_0, magnitude, _0)
            case Axis.Z => NonNullSpaceVector(_0, _0, magnitude)
        )

    def nonNullCrossProduct(v1: SpaceVector,
                            v2: SpaceVector): Option[NonNullSpaceVector] =
      val nx = v1.y * v2.z - v1.z * v2.y
      val ny = v1.z * v2.x - v1.x * v2.z
      val nz = v1.x * v2.y - v1.y * v2.x
      if SpaceVector.isNullTuple(nx, ny, nz) then
        None
      else
        Some(NonNullSpaceVector(nx, ny, nz))

    def fill(t: T): SpaceVector =
      SpaceVector(t, t, t)

    val Null = NullSpaceVector

    val OneX = NonNullSpaceVector.OneX
    val OneY = NonNullSpaceVector.OneY
    val OneZ = NonNullSpaceVector.OneZ


  /**
   * Base vectors are not coplanar
   */
  sealed trait Basis:
    self =>

    lazy val i: NonNullSpaceVector
    lazy val j: NonNullSpaceVector
    lazy val k: NonNullSpaceVector

    lazy val normalized: Basis =
      new Basis :
        override lazy val i: NonNullSpaceVector = self.i.normalized
        override lazy val j: NonNullSpaceVector = self.j.normalized
        override lazy val k: NonNullSpaceVector = self.k.normalized

    lazy val isNormalized: Boolean =
      Basis.normalized(i, j, k)

    lazy val isOrthogonal: Boolean =
      Basis.isOrthogonal(i, j, k)

    override def equals(obj: Any): Boolean =
      obj match
        case that: Basis if (that != null) =>
          (this.i == that.i) &&
            (this.j == that.j) &&
            (this.k == that.k)
        case _ =>
          false

    override def hashCode(): Int =
      31 * (
        31 * (
          i.##
        ) + j.##
      ) + k.##

    override def toString: String =
      s"Basis($i, $j, $k)"

  trait NormalizedBasis extends Basis :
    override lazy val normalized: NormalizedBasis =
      this

  trait OrthogonalBasis extends Basis :
    self =>
    override lazy val normalized: OrthonormalBasis =
      new OrthonormalBasis :
        override lazy val i: NonNullSpaceVector = self.i.normalized
        override lazy val j: NonNullSpaceVector = self.j.normalized
        override lazy val k: NonNullSpaceVector = self.k.normalized

  trait OrthonormalBasis extends OrthogonalBasis with NormalizedBasis :
    override lazy val normalized: OrthonormalBasis =
      this

  object Basis:

    def coplanar(i: NonNullSpaceVector,
                 j: NonNullSpaceVector,
                 k: NonNullSpaceVector): Boolean =
      ((i ∧ j) ∧ (i ∧ k)).isNull

    def isOrthogonal(i: NonNullSpaceVector,
                     j: NonNullSpaceVector,
                     k: NonNullSpaceVector): Boolean =
      (i ⋅ j == _0) && (i ⋅ k == _0) && (k ⋅ j == _0)

    def normalized(i: NonNullSpaceVector,
                   j: NonNullSpaceVector,
                   k: NonNullSpaceVector): Boolean =
      i.isNormalized && j.isNormalized && k.isNormalized

    def safe(i: SpaceVector,
             j: SpaceVector,
             k: SpaceVector): Option[Basis] =
      for
        baseI <- SpaceVector.nonNull(i)
        baseJ <- SpaceVector.nonNull(j)
        baseK <- SpaceVector.nonNull(k) if (!coplanar(baseI, baseJ, baseK))
      yield
        new Basis :
          override lazy val i: NonNullSpaceVector = baseI
          override lazy val j: NonNullSpaceVector = baseJ
          override lazy val k: NonNullSpaceVector = baseK

    def orthogonal(i: SpaceVector,
                   j: SpaceVector,
                   k: SpaceVector): Option[OrthogonalBasis] =
      for
        baseI <- SpaceVector.nonNull(i)
        baseJ <- SpaceVector.nonNull(j)
        baseK <- SpaceVector.nonNull(k)
        if (!coplanar(baseI, baseJ, baseK)) && isOrthogonal(baseI, baseJ, baseK)
      yield
        new OrthogonalBasis :
          override lazy val i: NonNullSpaceVector = baseI
          override lazy val j: NonNullSpaceVector = baseJ
          override lazy val k: NonNullSpaceVector = baseK

    def normalized(i: SpaceVector,
                   j: SpaceVector,
                   k: SpaceVector): Option[NormalizedBasis] =
      for
        baseI <- SpaceVector.nonNull(i)
        baseJ <- SpaceVector.nonNull(j)
        baseK <- SpaceVector.nonNull(k) if (!coplanar(baseI, baseJ, baseK))
      yield
        new NormalizedBasis :
          override lazy val i: NonNullSpaceVector = baseI.normalized
          override lazy val j: NonNullSpaceVector = baseJ.normalized
          override lazy val k: NonNullSpaceVector = baseK.normalized

    def orthonormal(i: SpaceVector,
                    j: SpaceVector): Option[OrthonormalBasis] =
      for
        baseI <- SpaceVector.nonNull(i)
        baseJ <- SpaceVector.nonNull(j) if (baseI ⋅ baseJ == _0)
        baseK <- SpaceVector.nonNullCrossProduct(baseI, baseJ)
      yield
        new OrthonormalBasis :
          override lazy val i: NonNullSpaceVector = baseI
          override lazy val j: NonNullSpaceVector = baseJ
          override lazy val k: NonNullSpaceVector = baseK

    val Normal: OrthonormalBasis =
      new OrthonormalBasis :
        override lazy val i: NonNullSpaceVector = Axis.X.base
        override lazy val j: NonNullSpaceVector = Axis.Y.base
        override lazy val k: NonNullSpaceVector = Axis.Z.base


  type Vertices = Iterable[Vertex]

  case class Vertex(s: SpacePoint,
                    t: SpacePoint):

    infix def +(v: SpaceVector): Vertex =
      Vertex(s + v, t + v)

  object Vertex:
    def apply(s: SpacePoint,
              v: SpaceVector): Vertex =
      Vertex(s, v.dest(s))

  trait Shape:
    val vertices: Vertices

    inline def +(s2: Shape): Shape =
      Shape.add(this, s2)

    override def toString: String =
      s"Shape(${vertices.mkString(",")})"

    override def equals(obj: Any): Boolean =
      obj match
        case that: Shape if that != null =>
          that.vertices == this.vertices
        case _ =>
          false

    override def hashCode(): Int =
      43 * vertices.hashCode()

  object Shape:
    def apply(vertices: Vertices): Shape =
      SimpleShape(vertices)

    def apply(vertices: Vertex*): Shape =
      SimpleShape(vertices)

    lazy val Empty: Shape =
      new Shape:
        override val vertices: Vertices =
          Iterable.empty[Vertex]

    def add(s1: Shape,
            s2: Shape): Shape =
      new Shape :
        override val vertices: Vertices =
          s1.vertices ++ s2.vertices


  private case class SimpleShape(vertices: Vertices)
    extends Shape
