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
  extends Model3D[T] with Cameras[T]:

  // Vector Transformations
  case class VectorRotation(angle: Radians,
                            axis: Vector) extends Rotation[Vector] :

    private val quaternion = Quaternion.fromRotationVector(axis, angle)

    final override def rotate(v: Vector): Option[Vector] =
      quaternion.flatMap(_.rotate(v))

  case class VectorTranslation(translation: Vector)
    extends Translation[Vector] :

    final override def translate(v: Vector): Vector =
      v + translation

  // end Vector Transformations


  // Point Transformations
  case class PointRotation(center: SpacePoint,
                           angle: Radians,
                           axis: Vector) extends Rotation[SpacePoint] :

    private val quaternion = Quaternion.fromRotationVector(axis, angle)

    final override def rotate(v: SpacePoint): Option[SpacePoint] =
      for
        vr <- quaternion.flatMap(_.rotate(Vector(center, v)))
      yield
        vr.dest(center)

  case class PointTranslation(translation: Vector)
    extends Translation[SpacePoint] :

    final override def translate(v: SpacePoint): SpacePoint =
      v + translation

  // end Point Transformations


  // Vertex Transformations
  case class VertexRotation(center: SpacePoint,
                            angle: Radians,
                            axis: Vector) extends Rotation[Vertex] :

    private val quaternion = Quaternion.fromRotationVector(axis, angle)

    final override def rotate(v: Vertex): Option[Vertex] =
      for
        vs <- quaternion.flatMap(_.rotate(Vector(center, v.s)))
        vt <- quaternion.flatMap(_.rotate(Vector(center, v.t)))
      yield
        Vertex(vs.dest(center), vt.dest(center))


  case class VertexTranslation(translation: Vector)
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

 

class Double3D(using Precision[Double]) extends Ez3D[Double]

class Float3D(using Precision[Float]) extends Ez3D[Float]

class BigDecimal3D(using Precision[BigDecimal]) extends Ez3D[BigDecimal]

class Real3D(using Precision[Real]) extends Ez3D[Real]