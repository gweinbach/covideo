/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.Precision
import spire.*
import spire.algebra.Trig
import spire.implicits.*
import spire.math.*

/**
 * @since 0.2.0
 * @author gweinbach on 12/06/2022
 */
trait Projection[T: Numeric : Trig : Precision]
  extends Space[T] with Plane[T] with TransformationMatrix[T] :


  sealed trait ProjectionViewFrustum:
    val nearDistance: T
    val farDistance: T

  sealed trait ProjectionCamera:
    val position: Point
    val target: Point
    val look: NonNullVector
    val up: NonNullVector
    val right: NonNullVector
    val viewFrustum: ProjectionViewFrustum

    val eye = Vector(position)


  object Parallel:

    case class ViewFrustum(nearDistance: T,
                           farDistance: T,
                           width: T,
                           height: T)
      extends ProjectionViewFrustum

    case class Camera(position: Point,
                      look: NonNullVector,
                      up: NonNullVector,
                      right: NonNullVector,
                      viewFrustum: ViewFrustum)
      extends ProjectionCamera:
      override val target = look.dest(position)


  object Perspective:

    case class ViewFrustum(nearDistance: T,
                           farDistance: T,
                           aspectRatio: T,
                           heightAngle: T)
      extends ProjectionViewFrustum:
      lazy val widthAngle = heightAngle + aspectRatio

    case class LookAtCamera private(position: Point,
                                    look: NonNullVector,
                                    up: NonNullVector,
                                    right: NonNullVector,
                                    basis: OrthonormalBasis,
                                    viewFrustum: ViewFrustum)
      extends ProjectionCamera:

      def project(p: Point): PlanePoint = ???

      override val target = look.dest(position)
      
      lazy val translation = AffineTranslation(eye).inverse
      lazy val orientation = BasisRotation(basis)
      
      lazy val viewMatrix = orientation ×: translation
      

    object LookAtCamera:

      def safe(position: Point,
               target: Point,
               upVector: Vector,
               viewFrustum: ViewFrustum): Option[LookAtCamera] =
        for
          look <- Vector.nonNull(position, target)
          up <- Vector.nonNull(upVector)
          right <- Vector.nonNullCrossProduct(-up, look)
          basis <- Basis.orthogonal(right, up, -look)
        yield
          LookAtCamera(position, look, up, right, basis.normalized, viewFrustum)