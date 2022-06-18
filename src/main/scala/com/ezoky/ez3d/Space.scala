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

  sealed trait Axis:
    def base: NonNullVector

  object Axis:

    case object X extends Axis :
      override def base: NonNullVector = Vector.OneX

    case object Y extends Axis :
      override def base: NonNullVector = Vector.OneY

    case object Z extends Axis :
      override def base: NonNullVector = Vector.OneZ

  case class Point(x: T,
                   y: T,
                   z: T)
    extends Transformable[Point] :

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

    val OneX = Point(__1, _0, _0)
    val OneY = Point(_0, __1, _0)
    val OneZ = Point(_0, _0, __1)


  sealed trait Vector
    extends Transformable[Vector] :
    val x: T
    val y: T
    val z: T

    final lazy val tuple: (T, T, T) =
      (x, y, z)

    final lazy val isNull: Boolean =
      Vector.isNullTuple(x, y, z)

    final lazy val magnitude: T =
      _SpatialNumeric.sqrt(this ⋅ this)

    final lazy val isNormalized: Boolean =
      magnitude == __1

    final def dest(origin: Point = Point.Zero): Point =
      Point(origin.x + x, origin.y + y, origin.z + z)

    def unary_- : Vector

    def isCollinear(v: Vector): Boolean

    lazy val inverse: Option[Vector]

    infix def +(v: Vector): Vector =
      Vector(x + v.x, y + v.y, z + v.z)

    infix def -(v: Vector): Vector =
      Vector(x - v.x, y - v.y, z - v.z)

    infix def *(n: T): Vector

    protected def divideBy(n: T): Vector

    infix def /(n: T): Option[Vector]

    infix def ⋅(v: Vector): T =
      x * v.x + y * v.y + z * v.z

    infix def ∧(v: Vector): Vector

    override def equals(obj: Any): Boolean =
      obj match
        case that: Vector if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y) &&
            (this.z ~= that.z)
        case _ =>
          false


  case object NullVector
    extends Vector :
    override val x: T = _0
    override val y: T = _0
    override val z: T = _0

    override inline def unary_- = NullVector

    override def isCollinear(v: Vector): Boolean =
      true

    override lazy val inverse: None.type =
      None

    override def +(v: Vector): Vector =
      v match
        case NullVector =>
          NullVector
        case _ =>
          v

    override def -(v: Vector): Vector =
      v match
        case NullVector =>
          NullVector
        case _ =>
          -v

    override def *(n: T): NullVector.type =
      NullVector

    override protected def divideBy(n: T): NullVector.type =
      NullVector

    override def /(n: T): Option[NullVector.type] =
      if n == _0 then
        None
      else
        Some(NullVector)

    override infix def ⋅(v: Vector): T =
      _0

    override infix def ∧(v: Vector): NullVector.type =
      NullVector

  case class NonNullVector(x: T,
                           y: T,
                           z: T)
    extends Vector :

    lazy val normalized: NonNullVector =
      divideBy(magnitude)

    override inline def unary_- =
      NonNullVector(-x, -y, -z)

    override def isCollinear(v: Vector): Boolean =
      (this ∧ v).isNull

    override lazy val inverse: Option[NonNullVector] =
      if (x ~= _0) || (y ~= _0) || (z ~= _0) then
        None
      else
        Some(NonNullVector(__1 / x, __1 / y, __1 / z))

    override def *(n: T): Vector =
      if n == _0 then
        NullVector
      else
        NonNullVector(
          x * n,
          y * n,
          z * n
        )

    override protected def divideBy(n: T): NonNullVector =
      NonNullVector(
        x / n,
        y / n,
        z / n
      )

    override final def /(n: T): Option[NonNullVector] =
      if n == _0 then
        None
      else
        Some(
          divideBy(n)
        )

    override final infix def ∧(v: Vector): Vector =
      Vector.nonNullCrossProduct(this, v).fold(NullVector)(v => v)


  object Vector:

    def isNullTuple(x: T, y: T, z: T): Boolean =
      (x ~= _0) && (y ~= _0) && (z ~= _0)

    inline def apply(x: T, y: T, z: T): Vector =
      nonNull(x, y, z).fold(NullVector)(v => v)

    inline def apply(start: Point,
                     end: Point): Vector =
      nonNull(start, end).fold(NullVector)(v => v)

    inline def apply(end: Point): Vector =
      nonNull(end).fold(NullVector)(v => v)

    inline def apply(magnitude: T,
                     axis: Axis): Vector =
      nonNull(magnitude, axis).fold(NullVector)(v => v)

    def nonNull(x: T,
                       y: T,
                       z: T): Option[NonNullVector] =
      if isNullTuple(x, y, z) then
        None
      else
        Some(NonNullVector(x, y, z))

    def nonNull(start: Point,
                       end: Point): Option[NonNullVector] =
      if start == end then
        None
      else
        Some(NonNullVector(end.x - start.x, end.y - start.y, end.z - start.z))

    def nonNull(end: Point): Option[NonNullVector] =
      nonNull(Point.Zero, end)

    def nonNull(v: Vector): Option[NonNullVector] =
      if v.isNull then
        None
      else
        Some(NonNullVector(v.x, v.y, v.z))

    def nonNull(magnitude: T,
                       axis: Axis): Option[NonNullVector] =
      if magnitude == _0 then
        None
      else
        Some(
          axis match
            case Axis.X => NonNullVector(magnitude, _0, _0)
            case Axis.Y => NonNullVector(_0, magnitude, _0)
            case Axis.Z => NonNullVector(_0, _0, magnitude)
        )

    def nonNullCrossProduct(v1: Vector,
                            v2: Vector): Option[NonNullVector] =
      val nx = v1.y * v2.z - v1.z * v2.y
      val ny = v1.z * v2.x - v1.x * v2.z
      val nz = v1.x * v2.y - v1.y * v2.x
      if Vector.isNullTuple(nx, ny, nz) then
        None
      else
        Some(NonNullVector(nx, ny, nz))

    def fill(t: T): Vector =
      Vector(t, t, t)

    val Null = NullVector

    val OneX = NonNullVector(__1, _0, _0)
    val OneY = NonNullVector(_0, __1, _0)
    val OneZ = NonNullVector(_0, _0, __1)


  /**
   * Base vectors are not coplanar
   */
  sealed trait Basis:
    self =>

    lazy val i: NonNullVector
    lazy val j: NonNullVector
    lazy val k: NonNullVector

    lazy val normalized: Basis =
      new Basis :
        override lazy val i: NonNullVector = self.i.normalized
        override lazy val j: NonNullVector = self.j.normalized
        override lazy val k: NonNullVector = self.k.normalized

    lazy val isNormalized: Boolean =
      Basis.normalized(i, j, k)

    lazy val isOrthogonal: Boolean =
      Basis.orthogonal(i, j, k)

    override def equals(obj: Any): Boolean =
      obj match
        case that: Basis if (that != null) =>
          (this.i == that.i) &&
            (this.j == that.j) &&
            (this.k == that.k)
        case _ =>
          false

  trait NormalizedBasis extends Basis :
    override lazy val normalized: NormalizedBasis =
      this

  trait OrthogonalBasis extends Basis :
    self =>
    override lazy val normalized: OrthonormalBasis =
      new OrthonormalBasis :
        override lazy val i: NonNullVector = self.i.normalized
        override lazy val j: NonNullVector = self.j.normalized
        override lazy val k: NonNullVector = self.k.normalized

  trait OrthonormalBasis extends OrthogonalBasis with NormalizedBasis :
    override lazy val normalized: OrthonormalBasis =
      this

  object Basis:

    def coplanar(i: NonNullVector,
                 j: NonNullVector,
                 k: NonNullVector): Boolean =
      ((i ∧ j) ∧ (i ∧ k)).isNull

    def orthogonal(i: NonNullVector,
                   j: NonNullVector,
                   k: NonNullVector): Boolean =
      (i ⋅ j == _0) && (i ⋅ k == _0) && (k ⋅ j == _0)

    def normalized(i: NonNullVector,
                   j: NonNullVector,
                   k: NonNullVector): Boolean =
      i.isNormalized && j.isNormalized && k.isNormalized

    def safe(i: Vector,
             j: Vector,
             k: Vector): Option[Basis] =
      for
        baseI <- Vector.nonNull(i)
        baseJ <- Vector.nonNull(j)
        baseK <- Vector.nonNull(k) if (!coplanar(baseI, baseJ, baseK))
      yield
        new Basis :
          override lazy val i: NonNullVector = baseI
          override lazy val j: NonNullVector = baseJ
          override lazy val k: NonNullVector = baseK

    def orthogonal(i: Vector,
                   j: Vector,
                   k: Vector): Option[OrthogonalBasis] =
      for
        baseI <- Vector.nonNull(i)
        baseJ <- Vector.nonNull(j)
        baseK <- Vector.nonNull(k)
        if (!coplanar(baseI, baseJ, baseK)) && orthogonal(baseI, baseJ, baseK)
      yield
        new OrthogonalBasis :
          override lazy val i: NonNullVector = baseI
          override lazy val j: NonNullVector = baseJ
          override lazy val k: NonNullVector = baseK

    def normalized(i: Vector,
                   j: Vector,
                   k: Vector): Option[NormalizedBasis] =
      for
        baseI <- Vector.nonNull(i)
        baseJ <- Vector.nonNull(j)
        baseK <- Vector.nonNull(k) if (!coplanar(baseI, baseJ, baseK))
      yield
        new NormalizedBasis :
          override lazy val i: NonNullVector = baseI.normalized
          override lazy val j: NonNullVector = baseJ.normalized
          override lazy val k: NonNullVector = baseK.normalized

    def orthonormal(i: Vector,
                    j: Vector): Option[OrthonormalBasis] =
      for
        baseI <- Vector.nonNull(i)
        baseJ <- Vector.nonNull(j) if (baseI ⋅ baseJ == _0)
        baseK <- Vector.nonNullCrossProduct(baseI, baseJ)
      yield
        new OrthonormalBasis :
          override lazy val i: NonNullVector = baseI
          override lazy val j: NonNullVector = baseJ
          override lazy val k: NonNullVector = baseK

    val Normal: OrthonormalBasis =
      new OrthonormalBasis :
        override lazy val i: NonNullVector = Axis.X.base
        override lazy val j: NonNullVector = Axis.Y.base
        override lazy val k: NonNullVector = Axis.Z.base


  type Vertices = Iterable[Vertex]

  case class Vertex(s: Point,
                    t: Point)
    extends Transformable[Vertex] :

    infix def +(v: Vector): Vertex =
      Vertex(s + v, t + v)


  object Vertex:
    def apply(s: Point, v: Vector): Vertex =
      Vertex(s, v.dest(s))

