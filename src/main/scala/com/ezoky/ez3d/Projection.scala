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
trait Projection[T: Numeric: Trig: Precision]
    extends Space[T]:


  sealed trait ProjectionViewFrustum:
    val nearDistance: T
    val farDistance: T

  sealed trait ProjectionCamera:
    val position: Point
    val lookVector: Vector
    val upVector: Vector
    val viewFrustum: ProjectionViewFrustum


  object Parallel:

    case class ViewFrustum(nearDistance: T,
                           farDistance: T,
                           width: T,
                           height: T)
      extends ProjectionViewFrustum

    case class Camera(position: Point,
                      lookVector: Vector,
                      upVector: Vector,
                      viewFrustum: ViewFrustum)
      extends ProjectionCamera


  object Perspective:

    case class ViewFrustum(nearDistance: T,
                           farDistance: T,
                           aspectRatio: T,
                           heightAngle: T)
      extends ProjectionViewFrustum:
      lazy val widthAngle = heightAngle + aspectRatio

    case class Camera(position: Point,
                      lookVector: Vector,
                      upVector: Vector,
                      viewFrustum: ViewFrustum)
      extends ProjectionCamera