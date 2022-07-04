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
                            axis: NonNullSpaceVector) extends Rotation[SpaceVector] :

    private val quaternion = Quaternion.fromRotationVectorAndAngle(axis, angle)

    final override def rotate(v: SpaceVector): Option[SpaceVector] =
      quaternion.rotate(v)

  case class VectorTranslation(translation: SpaceVector)
    extends Translation[SpaceVector] :

    final override def translate(v: SpaceVector): SpaceVector =
      v + translation

  // end Vector Transformations


  // Point Transformations
  case class PointRotation(center: SpacePoint,
                           angle: Radians,
                           axis: NonNullSpaceVector) extends Rotation[SpacePoint] :

    private val quaternion = Quaternion.fromRotationVectorAndAngle(axis, angle)

    final override def rotate(v: SpacePoint): Option[SpacePoint] =
      quaternion.rotate(SpaceVector(center, v)).map(_.dest(center))


  case class PointTranslation(translation: SpaceVector)
    extends Translation[SpacePoint] :

    final override def translate(v: SpacePoint): SpacePoint =
      v + translation

  // end Point Transformations


  // Vertex Transformations
  case class VertexRotation(center: SpacePoint,
                            angle: Radians,
                            axis: NonNullSpaceVector) extends Rotation[Vertex] :

    private val quaternion = Quaternion.fromRotationVectorAndAngle(axis, angle)

    final override def rotate(v: Vertex): Option[Vertex] =
      for
        rs <- quaternion.rotate(SpaceVector(center, v.s))
        rt <- quaternion.rotate(SpaceVector(center, v.t))
      yield
        Vertex(rs.dest(center), rt.dest(center))


  case class VertexTranslation(translation: SpaceVector)
    extends Translation[Vertex] :

    final override def translate(v: Vertex): Vertex =
      v + translation

  // end Vertex Transformations


  import Perspective.*

  given Model[Camera] with
    extension (camera: Camera)
      override def position: SpacePoint = camera.position
      override def basis: Basis = camera.basis

  given ProjectionView[Camera] with
    extension (camera: Camera)
      override def projectionMatrix: Matrix = camera.viewFrustum.projectionMatrix


given [T: Numeric: Trig: Precision]:Ez3D[T] = new Ez3D[T]