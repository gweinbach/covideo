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


/**
 * @since $NEXT_VERSION
 * @author gweinbach on 10/06/2022
 */
class Ez3D[T: Numeric : Trig : Precision]
  extends Model3D[T] with Cameras[T] with Shapes[T]:

  // Vector Transformations
  case class VectorRotation(angle: Radians,
                            axis: NonNullSpaceVector) extends Rotation[SpaceVector]:

    private val quaternion =
      Quaternion.fromRotationVectorAndAngle(axis, angle)

    final override def rotate(v: SpaceVector): SpaceVector =
      quaternion.rotate(v)

  case class VectorTranslation(translation: SpaceVector)
    extends Translation[SpaceVector]:

    final override def translate(v: SpaceVector): SpaceVector =
      v + translation

  // end Vector Transformations


  // Point Transformations
  case class PointRotation(center: SpacePoint,
                           angle: Radians,
                           axis: NonNullSpaceVector) extends Rotation[SpacePoint]:

    private val quaternion = Quaternion.fromRotationVectorAndAngle(axis, angle)

    final override def rotate(v: SpacePoint): SpacePoint =
      quaternion.rotate(SpaceVector(center, v)).dest(center)


  case class PointTranslation(translation: SpaceVector)
    extends Translation[SpacePoint]:

    final override def translate(v: SpacePoint): SpacePoint =
      v + translation

  // end Point Transformations


  // Segment Transformations
  case class SegmentRotation(center: SpacePoint,
                             angle: Radians,
                             axis: NonNullSpaceVector) extends Rotation[Segment]:

    private val quaternion = Quaternion.fromRotationVectorAndAngle(axis, angle)

    final override def rotate(v: Segment): Segment =
      Segment(
        quaternion.rotate(SpaceVector(center, v.s)).dest(center),
        quaternion.rotate(SpaceVector(center, v.t)).dest(center)
      )


  case class SegmentTranslation(translation: SpaceVector)
    extends Translation[Segment]:

    final override def translate(v: Segment): Segment =
      v + translation

  // end Segment Transformations


  //  import Perspective.*

  given Model[Camera] with
    extension (camera: Camera)
      override def position: SpacePoint = camera.position
      override def basis: Basis = camera.basis

  given ProjectionView[Camera] with
    extension (camera: Camera)
      override def projectionMatrix: Matrix = camera.viewFrustum.projectionMatrix

/**
 * Automatically provides an implicit Ez3D instance for any spire Numeric type
 * 
 * @tparam T any Spire Numeric type with a given [[com.ezoky.eznumber.Precision]]
 * @return an implicit Ez3D instance
 */
given [T: Numeric : Trig : Precision]: Ez3D[T] = new Ez3D[T]