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
  extends Vectors[T] with H[T] :

  // Vector Transformations
  case class VectorRotation(angle: T,
                            axis: Vector) extends Rotation[Vector]:

    private val quaternion = Quaternion.fromRotationVector(axis, angle)

    final override def rotate(v: Vector): Option[Vector] =
      quaternion.flatMap(_.rotate(v))

  case class VectorTranslation(translation: Vector)
    extends Translation[Vector]:

    final override def translate(v: Vector): Vector =
      v + translation

  // end Vector Transformations


  // Point Transformations
  case class PointRotation(center: Point,
                            angle: T,
                            axis: Vector) extends Rotation[Point]:

    private val quaternion = Quaternion.fromRotationVector(axis, angle)

    final override def rotate(v: Point): Option[Point] =
      for
        vr <- quaternion.flatMap(_.rotate(Vector(center, v)))
      yield
        vr.dest(center)

  case class PointTranslation(translation: Vector)
    extends Translation[Point]:

    final override def translate(v: Point): Point =
      v + translation

  // end Point Transformations


  // Vertex Transformations
  case class VertexRotation(center: Point,
                            angle: T,
                            axis: Vector) extends Rotation[Vertex]:

    private val quaternion = Quaternion.fromRotationVector(axis, angle)

    final override def rotate(v: Vertex): Option[Vertex] =
      for
        vs <- quaternion.flatMap(_.rotate(Vector(center, v.s)))
        vt <- quaternion.flatMap(_.rotate(Vector(center, v.t)))
      yield
        Vertex(vs.dest(center), vt.dest(center))


  case class VertexTranslation(translation: Vector)
    extends Translation[Vertex]:

    final override def translate(v: Vertex): Vertex =
      v + translation

  // end Vertex Transformations


class Double3D(using Precision[Double]) extends Ez3D[Double]
class Float3D(using Precision[Float]) extends Ez3D[Float]
class BigDecimal3D(using Precision[BigDecimal]) extends Ez3D[BigDecimal]
class Real3D(using Precision[Real]) extends Ez3D[Real]