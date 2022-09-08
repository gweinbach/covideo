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

    println(s"viewFrustum: near=$near, far=$far, depth = $depth")

    val top: T
    val bottom: T
    val right: T
    val left: T

    val near: T
    val far: T

    protected def setNearAndFar(near: T,
                                far: T): ViewFrustum

    final def withNear(newNear: T): ViewFrustum =
      if newNear == this.near then
        this
      else
        val near = if (newNear > _0) then newNear else this.near
        val far = near + depth
        setNearAndFar(near = near, far = far)

    final def withFar(newFar: T): ViewFrustum =
      if newFar == this.far then
        this
      else
        val far = if (newFar - depth > _0) then newFar else this.far
        val near = far - depth
        setNearAndFar(near = near, far = far)


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
          Segment(nearBottomLeft, nearTopLeft),
          Segment(nearTopLeft, nearTopRight),
          Segment(nearTopRight, nearBottomRight),
          Segment(nearBottomRight, nearBottomLeft),

          Segment(farBottomLeft, farTopLeft),
          Segment(farTopLeft, farTopRight),
          Segment(farTopRight, farBottomRight),
          Segment(farBottomRight, farBottomLeft),

          Segment(nearBottomLeft, farBottomLeft),
          Segment(nearTopLeft, farTopLeft),
          Segment(nearTopRight, farTopRight),
          Segment(nearBottomRight, farBottomRight)
        )
      )

  sealed trait SymetricViewFrustum
    extends ViewFrustum:

    final override val bottom = -top
    final override val left = -right


  private[ez3d] trait DefaultViewFrustum
    extends ViewFrustum :

    val top: T = __1
    val bottom: T = -__1
    val right: T = __1
    val left: T = -__1

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
      extends DefaultViewFrustum:

      final override protected def setNearAndFar(near: T,
                                                 far: T): SimpleViewFrustum =
        copy(near = near, far = far)


    val Default = SimpleViewFrustum(
      near = __1,
      far = __2
    )

  sealed trait Camera:

    // World coordinates
    val position: SpacePoint
    val target: SpacePoint
    val basis: Basis

    final lazy val eye: SpaceVector = SpaceVector(position)
    final lazy val look = SpaceVector(position, target)
    final lazy val right = basis.i
    final lazy val up = basis.j

    val viewFrustum: ViewFrustum

    def withPosition(newPosition: SpacePoint): Camera

    def withTarget(newTarget: SpacePoint): Camera

    def withBasis(newBasis: Basis): Camera

    def withViewFrustum(newViewFrustum: ViewFrustum): Camera

    def move(dx: T,
             dy: T): Camera

  object Camera:

    private case class SimpleCamera(position: SpacePoint = SpacePoint.Origin,
                                    target: SpacePoint = SpacePoint(_0, _0, -__1),
                                    basis: Basis = Basis.NormalDirect,
                                    viewFrustum: ViewFrustum = ViewFrustum.Default)
      extends Camera :
      final override def withPosition(newPosition: SpacePoint): Camera =
        copy(position = newPosition)

      final override def withTarget(newTarget: SpacePoint): Camera =
        copy(target = newTarget)

      final override def withBasis(newBasis: Basis): Camera =
        copy(basis = newBasis)

      final override def withViewFrustum(newViewFrustum: ViewFrustum): Camera =
        copy(viewFrustum = newViewFrustum)

      final override def move(dx: T, dy: T): Camera =
        copy(position = position.withX(position.x + dx).withY(position.y + dy))

    val Default: Camera = SimpleCamera()


  case class LookAtCamera private(position: SpacePoint,
                                  target: SpacePoint,
                                  basis: Basis,
                                  viewFrustum: ViewFrustum)
    extends Camera :

    final override def withPosition(newPosition: SpacePoint): Camera =
      copy(position = newPosition)

    final override def withTarget(newTarget: SpacePoint): Camera =
      copy(target = newTarget)

    final override def withBasis(newBasis: Basis): Camera =
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
        (for
          vector <- SpaceVector.nonNull(target, position)
          rotation = qh × qv
          rotated = rotation.rotate(vector)
          rotatedBasis <- rotation.rotate(basis)
        yield
          copy(
            position = rotated.dest(target),
            basis = rotatedBasis
          )).getOrElse(this) // TODO handle error

  object LookAtCamera:

    def safe(position: SpacePoint,
             target: SpacePoint,
             upVector: SpaceVector,
             viewFrustum: ViewFrustum): Option[LookAtCamera] =
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

  sealed trait Projection:

    protected def viewFrustumFromSymetricPlanes(nearDistance: T,
                                                farDistance: T,
                                                topDistance: T,
                                                rightDistance: T): Option[ViewFrustum]
    /**
     * The projection screen is located on top side of the box.
     */
    def viewBoxFromTop(sceneWidth: T,
                       sceneHeight: T,
                       sceneDepth: T,
                       cameraDistance: T): Option[Camera] =
      for
        viewFrustum <-
          viewFrustumFromSymetricPlanes(
            nearDistance = cameraDistance,
            farDistance = cameraDistance + sceneDepth,
            topDistance = sceneHeight / __2,
            rightDistance = sceneWidth / __2
          )
        cameraPosition = SpacePoint(x = sceneWidth / __2, y = sceneHeight / __2, z = cameraDistance)
        targetPosition = cameraPosition.withZ(-sceneDepth / __2) // Center of the box
        camera <- LookAtCamera.safe(
          position = cameraPosition,
          target = targetPosition,
          upVector = SpaceVector.OneY,
          viewFrustum
        )
      yield
        camera

    /**
     * The projection screen is located on letft side of the box.
     */
    def viewBoxFromLeft(sceneWidth: T,
                        sceneHeight: T,
                        sceneDepth: T,
                        cameraDistance: T): Option[Camera] =
      for
        viewFrustum <-
          viewFrustumFromSymetricPlanes(
            nearDistance = cameraDistance,
            farDistance = cameraDistance + sceneWidth,
            topDistance = sceneDepth / __2,
            rightDistance = sceneHeight / __2
          )
        cameraPosition = SpacePoint(x = cameraDistance, y = sceneHeight / __2, z = -sceneDepth / __2)
        targetPosition = cameraPosition.withX(sceneWidth / 2) // Center of the box
        camera <- LookAtCamera.safe(
          position = cameraPosition,
          target = targetPosition,
          upVector = SpaceVector.OneY,
          viewFrustum
        )
      yield
        camera


  object Orthographic
    extends Projection:

    trait OrthographicViewFrustum
      extends ViewFrustum :

      final override lazy val projectionMatrix =
        Matrix(
          y00 = __2 / (right - left),
          y01 = _0,
          y02 = _0,
          y03 = (right + left) / (left - right),

          y10 = _0,
          y11 = __2 / (top - bottom),
          y12 = _0,
          y13 = (top + bottom) / (bottom - top),

          y20 = _0,
          y21 = _0,
          y22 = __2 / (near - far),
          y23 = (far + near) / (near - far),

          y30 = _0,
          y31 = _0,
          y32 = _0,
          y33 = __1
        )

      final override lazy val minXn: T = left
      final override lazy val minXf: T = left
      final override lazy val maxXn: T = right
      final override lazy val maxXf: T = right

      final override lazy val minYn: T = bottom
      final override lazy val minYf: T = bottom
      final override lazy val maxYn: T = top
      final override lazy val maxYf: T = top

      final override lazy val minZ: T = -far
      final override lazy val maxZ: T = -near
      final override lazy val middleZ: T = (near - far) / __2

    private case class SymetricOrthographicViewFrustum(near: T,
                                                       far: T,
                                                       top: T,
                                                       right: T)
      extends OrthographicViewFrustum
        with SymetricViewFrustum:

      final override protected def setNearAndFar(near: T,
                                                 far: T): SymetricOrthographicViewFrustum =
         copy(near = near, far = far)

    final override def viewFrustumFromSymetricPlanes(nearDistance: T,
                                                     farDistance: T,
                                                     topDistance: T,
                                                     rightDistance: T): Option[ViewFrustum] =
      if (nearDistance <= _0) ||
        (farDistance <= nearDistance) ||
        (topDistance <= _0) ||
        (rightDistance <= _0) then
        None
      else
        Some(
          SymetricOrthographicViewFrustum(
            near = nearDistance,
            far = farDistance,
            top = topDistance,
            right = rightDistance
          )
        )




  object Perspective
    extends Projection:

    trait PerspectiveViewFrustum
      extends ViewFrustum :

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


    private case class SymetricPerspectiveViewFrustum(near: T,
                                                      far: T,
                                                      top: T,
                                                      right: T)
      extends PerspectiveViewFrustum
        with SymetricViewFrustum :

      final override protected def setNearAndFar(near: T,
                                                 far: T): SymetricPerspectiveViewFrustum =
        copy(near = near, far = far)


    final override def viewFrustumFromSymetricPlanes(nearDistance: T,
                                                     farDistance: T,
                                                     topDistance: T,
                                                     rightDistance: T): Option[ViewFrustum] =
      if (nearDistance <= _0) ||
        (farDistance <= nearDistance) ||
        (topDistance <= _0) ||
        (rightDistance <= _0) then
        None
      else
        Some(
          SymetricPerspectiveViewFrustum(
            near = nearDistance,
            far = farDistance,
            top = topDistance,
            right = rightDistance
          )
        )

    def viewFrustumFromSymetricFieldOfView(nearDistance: T,
                                           farDistance: T,
                                           aspectRatio: T,
                                           height: Radians): Option[ViewFrustum] =
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
            SymetricPerspectiveViewFrustum(
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