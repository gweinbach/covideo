/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.{Angles, Precision}
import spire.*
import spire.algebra.Trig
import spire.implicits.*
import spire.math.*

/**
 * @since 0.2.0
 * @author gweinbach on 12/06/2022
 */
trait Cameras[T: Numeric : Trig : Precision]
  extends Space[T]
    with Plane[T]
    with Transformation3D[T]
    with Angles[T] :

  private val _Numeric = summon[Numeric[T]]
  private val _0 = _Numeric.zero
  private val __1 = _Numeric.one // double '_' to avoid conflict with Product<X>._1
  private val __2 = _Numeric.fromInt(2) // double '_' to avoid conflict with Product<X>._1

  sealed trait ViewFrustum:

    lazy val near: T
    lazy val far: T
    lazy val projectionMatrix: Matrix

    lazy val minXn: T
    lazy val minXf: T
    lazy val maxXn: T
    lazy val maxXf: T

    lazy val minYn: T
    lazy val minYf: T
    lazy val maxYn: T
    lazy val maxYf: T

    lazy val minZ: T
    lazy val maxZ: T
    lazy val middleZ: T

    final lazy val nearBottomLeft = SpacePoint(minXn, minYn, maxZ)
    final lazy val nearTopLeft = SpacePoint(minXn, maxYn, maxZ)
    final lazy val nearBottomRight = SpacePoint(maxXn, minYn, maxZ)
    final lazy val nearTopRight = SpacePoint(maxXn, maxYn, maxZ)

    final lazy val farBottomLeft = SpacePoint(minXf, minYf, minZ)
    final lazy val farTopLeft = SpacePoint(minXf, maxYf, minZ)
    final lazy val farBottomRight = SpacePoint(maxXf, minYf, minZ)
    final lazy val farTopRight = SpacePoint(maxXf, maxYf, minZ)

    final lazy val center = SpacePoint(_0, _0, middleZ)

    final lazy val shape: Shape =
      Shape(
        Vector(
          Vertex(nearBottomLeft, nearTopLeft),
          Vertex(nearTopLeft, nearTopRight),
          Vertex(nearTopRight, nearBottomRight),
          Vertex(nearBottomRight, nearBottomLeft),

          Vertex(farBottomLeft, farTopLeft),
          Vertex(farTopLeft, farTopRight),
          Vertex(farTopRight, farBottomRight),
          Vertex(farBottomRight, farBottomLeft),

          Vertex(nearBottomLeft, farBottomLeft),
          Vertex(nearTopLeft, farTopLeft),
          Vertex(nearTopRight, farTopRight),
          Vertex(nearBottomRight, farBottomRight)
        )
      )


  object ViewFrustum:
    val Default = new ViewFrustum :
      override lazy val near: T = __1
      override lazy val far: T = __2
      override lazy val projectionMatrix: Matrix = Matrix.Identity

      final lazy val minXn: T = -__1
      final lazy val minXf: T = -__1
      final lazy val maxXn: T = __1
      final lazy val maxXf: T = __1

      final lazy val minYn: T = -__1
      final lazy val minYf: T = -__1
      final lazy val maxYn: T = __1
      final lazy val maxYf: T = __1

      final lazy val minZ: T = -near
      final lazy val maxZ: T = -far
      final lazy val middleZ: T = (minZ + maxZ) / __2


  sealed trait Camera:
    val position: SpacePoint
    val look: NonNullSpaceVector
    val up: NonNullSpaceVector
    val right: NonNullSpaceVector
    val basis: OrthonormalBasis
    val viewFrustum: ViewFrustum

    final lazy val eye: SpaceVector = SpaceVector(position)
    final lazy val target: SpacePoint = look.dest(position)

  //  object Parallel:

  //    case class ViewFrustum(near: T,
  //                           far: T,
  //                           width: T,
  //                           height: T)
  //      extends ProjectionViewFrustum

  //    case class Camera(position: Point,
  //                      look: NonNullVector,
  //                      up: NonNullVector,
  //                      right: NonNullVector,
  //                      viewFrustum: ViewFrustum)
  //      extends ProjectionCamera :
  //      override val target = look.dest(position)

  object Camera:
    val Default: Camera =
      new Camera :
        override val position: SpacePoint = SpacePoint.Origin
        override val look: NonNullSpaceVector = -NonNullSpaceVector.OneZ
        override val up: NonNullSpaceVector = NonNullSpaceVector.OneY
        override val right: NonNullSpaceVector = NonNullSpaceVector.OneX
        override val basis: OrthonormalBasis = Basis.Normal
        override val viewFrustum: ViewFrustum = ViewFrustum.Default

  object Perspective:

    trait PerspectiveViewFrustum
      extends ViewFrustum :

      lazy val top: T
      lazy val bottom: T
      lazy val right: T
      lazy val left: T

      final lazy val projectionMatrix =
        Matrix(
          y00 = __2 * near / (right - left),
          y01 = _0,
          y02 = (right + left) / (right - left),
          y03 = _0,

          y10 = _0,
          y11 = __2 * near / (top - bottom),
          y12 = (top + bottom) / (top - bottom),
          y13 = _0,

          y20 = _0,
          y21 = _0,
          y22 = (far + near) / (near - far),
          y23 = __2 * far * near / (near - far),

          y30 = _0,
          y31 = _0,
          y32 = -__1,
          y33 = _0
        )

      final lazy val minXn: T = left
      final lazy val minXf: T = left * (far / near)
      final lazy val maxXn: T = right
      final lazy val maxXf: T = right * (far / near)

      final lazy val minYn: T = bottom
      final lazy val minYf: T = bottom * (far / near)
      final lazy val maxYn: T = top
      final lazy val maxYf: T = top * (far / near)

      final lazy val minZ: T = -far
      final lazy val maxZ: T = -near
      final lazy val middleZ: T = -(__2 * far * near) / (far + near)


    trait SymetricViewFrustum
      extends PerspectiveViewFrustum :
      final override lazy val bottom = -top
      final override lazy val left = -right


    object ViewFrustum:
      def fromSymetricPlanes(nearDistance: T,
                             farDistance: T,
                             topDistance: T,
                             rightDistance: T): Option[SymetricViewFrustum] =
        if (nearDistance <= _0) ||
          (farDistance <= nearDistance) ||
          (topDistance <= 0) ||
          (rightDistance <= 0) then
          None
        else
          Some(
            new SymetricViewFrustum :
              final override lazy val near = nearDistance
              final override lazy val far = farDistance
              final override lazy val top = topDistance
              final override lazy val right = rightDistance
          )

      def fromSymetricFieldOfView(nearDistance: T,
                                  farDistance: T,
                                  aspectRatio: T,
                                  height: Radians): Option[SymetricViewFrustum] =
        if (nearDistance <= _0) ||
          (farDistance <= nearDistance) ||
          (aspectRatio <= _0) ||
          (height <= (_0 radians)) ||
          (height >= pi[Radians]) then
          None
        else
          Some(
            new SymetricViewFrustum :
              final override lazy val near = nearDistance
              final override lazy val far = farDistance

              final lazy val focalLength = __1 / tan(height / (__2 radians))

              final override lazy val top = near / focalLength
              final override lazy val right = aspectRatio * top

            // used for control
            //              final lazy val alternateProjectionMatrix =
            //                Matrix(
            //                  y00 = focalLength / aspectRatio,
            //                  y01 = _0,
            //                  y02 = _0,
            //                  y03 = _0,
            //
            //                  y10 = _0,
            //                  y11 = focalLength,
            //                  y12 = _0,
            //                  y13 = _0,
            //
            //                  y20 = _0,
            //                  y21 = _0,
            //                  y22 = (far + near) / (near - far),
            //                  y23 = __2 * far * near / (near - far),
            //
            //                  y30 = _0,
            //                  y31 = _0,
            //                  y32 = -__1,
            //                  y33 = _0
            //                )
            //              assert(projectionMatrix == alternateProjectionMatrix)
          )

    case class LookAtCamera private(position: SpacePoint,
                                    look: NonNullSpaceVector,
                                    up: NonNullSpaceVector,
                                    right: NonNullSpaceVector,
                                    basis: OrthonormalBasis,
                                    viewFrustum: PerspectiveViewFrustum)
      extends Camera


    object LookAtCamera:

      def safe(position: SpacePoint,
               target: SpacePoint,
               upVector: SpaceVector,
               viewFrustum: PerspectiveViewFrustum): Option[LookAtCamera] =
        for
          look <- SpaceVector.nonNull(position, target)
          up <- SpaceVector.nonNull(upVector)
          right <- SpaceVector.nonNullCrossProduct(-up, look)
          basis <- Basis.orthogonal(right, up, -look)
        yield
          LookAtCamera(
            position = position,
            look = look.normalized,
            up = up.normalized,
            right = right.normalized,
            basis = basis.normalized,
            viewFrustum = viewFrustum
          )

//      for
//        look <- Vector.nonNull(position, target)
//        lookNormalized = look.normalized
//        up <- Vector.nonNull(upVector)
//        upNormalized = up.normalized
//        right <- Vector.nonNullCrossProduct(-upNormalized, lookNormalized)
//        rightNormalized = right.normalized
//        basis <- Basis.orthogonal(right, up, -look)
//      yield
//        LookAtCamera(
//          position = position,
//          look = lookNormalized,
//          up = upNormalized,
//          right = rightNormalized,
//          basis = basis.normalized,
//          viewFrustum = viewFrustum
//        )
