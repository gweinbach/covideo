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

    val near: T

    def withNear(near: T): ViewFrustum

    val far: T

    def withFar(far: T): ViewFrustum

    lazy val depth: T = far - near

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

  private[ez3d] trait DefaultViewFrustum
    extends ViewFrustum :

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

  object ViewFrustum:
    private[ez3d] case class SimpleViewFrustum(near: T,
                                               far: T)
      extends DefaultViewFrustum :
      final override def withNear(newNear: T): SimpleViewFrustum =
        copy(near =
          if (newNear > _0) then newNear else near
        )

      final override def withFar(newFar: T): SimpleViewFrustum =
        copy(far =
          if (newFar > near) then newFar else newFar
        )

    val Default = SimpleViewFrustum(
      near = __1,
      far = __2
    )

  sealed trait Camera:
    // World coordinates
    val position: SpacePoint
    val target: SpacePoint
    val basis: OrthonormalBasis
    final lazy val eye: SpaceVector = SpaceVector(position)
    final lazy val look = SpaceVector(position, target)
    final lazy val right = basis.i
    final lazy val up = basis.j

    val viewFrustum: ViewFrustum

    def withPosition(newPosition: SpacePoint): Camera

    def withTarget(newTarget: SpacePoint): Camera

    def withBasis(newBasis: OrthonormalBasis): Camera

    def withViewFrustum(newViewFrustum: ViewFrustum): Camera

    def move(dx: T,
             dy: T): Camera

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

    private case class SimpleCamera(position: SpacePoint = SpacePoint.Origin,
                                    target: SpacePoint = SpacePoint(_0, _0, -__1),
                                    basis: OrthonormalBasis = Basis.NormalDirect,
                                    viewFrustum: ViewFrustum = ViewFrustum.Default)
      extends Camera :
      final override def withPosition(newPosition: SpacePoint): Camera =
        copy(position = newPosition)
      final override def withTarget(newTarget: SpacePoint): Camera =
        copy(target = newTarget)
      final override def withBasis(newBasis: OrthonormalBasis): Camera =
        copy(basis = newBasis)
      final override def withViewFrustum(newViewFrustum: ViewFrustum): Camera =
        copy(viewFrustum = newViewFrustum)
      final override def move(dx: T, dy: T): Camera =
        copy(position = position.withX(position.x + dx).withY(position.y + dy))

    val Default: Camera = SimpleCamera()

  object Perspective:

    trait PerspectiveViewFrustum
      extends ViewFrustum :

      val top: T
      val bottom: T
      val right: T
      val left: T

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

      final lazy val focalLength: T = near / top

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


    case class SymetricViewFrustum private(near: T,
                                           far: T,
                                           top: T,
                                           right: T)
      extends PerspectiveViewFrustum :
      final override val bottom = -top
      final override val left = -right

      println(s"camera: near=$near, far=$far, depth = $depth")
      final override def withNear(newNear: T): SymetricViewFrustum =
        if newNear == this.near then
          this
        else
          val near = if (newNear > _0) then newNear else this.near
          val far = near + depth
          copy(near = near, far = far)

      final override def withFar(newFar: T): SymetricViewFrustum =
        if newFar == this.far then
          this
        else
          val far = if (newFar - depth > _0) then newFar else this.far
          val near = far - depth
          copy(near = near, far = far)


    object SymetricViewFrustum:
      def fromSymetricPlanes(nearDistance: T,
                             farDistance: T,
                             topDistance: T,
                             rightDistance: T): Option[SymetricViewFrustum] =
        if (nearDistance <= _0) ||
          (farDistance <= nearDistance) ||
          (topDistance <= _0) ||
          (rightDistance <= _0) then
          None
        else
          Some(
            SymetricViewFrustum(
              near = nearDistance,
              far = farDistance,
              top = topDistance,
              right = rightDistance
            )
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
          val focalLength = __1 / tan(height / (__2 radians))
          val topDistance = nearDistance / focalLength
          val rightDistance = aspectRatio * topDistance
          Some(
            SymetricViewFrustum(
              near = nearDistance,
              far = farDistance,
              top = topDistance,
              right = rightDistance
            )
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

    val ViewFrustum = SymetricViewFrustum

    case class LookAtCamera private(position: SpacePoint,
                                    target: SpacePoint,
                                    basis: OrthonormalBasis,
                                    viewFrustum: ViewFrustum)
      extends Camera :
      final override def withPosition(newPosition: SpacePoint): Camera =
        copy(position = newPosition)

      final override def withTarget(newTarget: SpacePoint): Camera =
        copy(target = newTarget)

      final override def withBasis(newBasis: OrthonormalBasis): Camera =
        copy(basis = newBasis)

      final override def withViewFrustum(newViewFrustum: ViewFrustum): Camera =
        copy(viewFrustum = newViewFrustum)

      final override def move(dx: T,
                              dy: T): Camera =
        if dx == _0 && dy == _0 then
          this
        else
          val targetDistance = look.magnitude
          val horizontalAngle: Radians = atan(dx / targetDistance)
          val verticalAngle: Radians = atan(dy / targetDistance)
          val qh = Quaternion.fromRotationVectorAndAngle(up, horizontalAngle)
          val qv = Quaternion.fromRotationVectorAndAngle(right, verticalAngle)
          val newPositionBasis =
            for
              vector <- SpaceVector.nonNull(target, position)
              rotation = qh × qv
              rotated = rotation.rotate(vector)
              rotatedBasis <- rotation.rotate(basis).asInstanceOf[Option[OrthonormalBasis]]
            yield
//              (rotated.dest(target), basis)
              (rotated.dest(target), rotatedBasis)
          copy(
            position = newPositionBasis.map(_._1).getOrElse(position),
            basis = newPositionBasis.map(_._2).getOrElse(basis)
          ) // TODO handle error

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
            target = target,
            basis = basis.normalized,
            viewFrustum = viewFrustum
          )

      def viewBoxFromTop(sceneWidth: T,
                         sceneHeight: T,
                         sceneDepth: T,
                         cameraDistance: T): Option[Camera] =
        for
          viewFrustum <-
            Perspective.ViewFrustum.fromSymetricPlanes(
              nearDistance = cameraDistance,
              farDistance = cameraDistance + sceneDepth,
              topDistance = sceneHeight / __2,
              rightDistance = sceneWidth / __2
            )
          cameraPosition = SpacePoint(x = sceneWidth / __2, y = sceneHeight / __2, z = cameraDistance)
          targetPosition = cameraPosition.withZ(-sceneDepth / __2) // Center of the box
          camera <- Perspective.LookAtCamera.safe(
              position = cameraPosition,
              target = targetPosition,
              upVector = SpaceVector.OneY,
              viewFrustum
            )
        yield
          camera


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
