/*
 * @author gweinbach on 18/06/2022 22:38
 * @since 0.2.0
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.Precision
import spire.*
import spire.implicits.*
import spire.math.*

/**
 * @since $NEXT_VERSION
 * @author gweinbach on 18/06/2022
 */
trait Model3D[T: Numeric : Precision]
  extends Space[T] with Transformation3D[T]
  with Plane[T] with Transformation2D[T]:

  private val _Numeric = summon[Numeric[T]]
  private val _0 = _Numeric.zero
  private val __1 = _Numeric.one // double '_' to avoid conflict with Product<X>._1
  private val __2 = _Numeric.fromInt(2) // double '_' to avoid conflict with Product<X>._1

  trait Model[M]:
    val position: Point
    val basis: Basis

    lazy val translation = AffineTranslation(Vector(position))
    lazy val orientation = BasisTransformation(basis)

    lazy val modelMatrix: Matrix = translation ×: orientation
    lazy val viewMatrix: Matrix = orientation.inverse ×: translation.inverse

    final def modelToWorld(modelPoint: Point): Option[Point] =
      (modelMatrix × modelPoint.homogeneous).cartesian

    final def worldToView(worldPoint: Point): Option[Point] =
      (viewMatrix × worldPoint.homogeneous).cartesian

  trait ProjectionView[M]:
    def projectionMatrix: Matrix

    final def viewToClip(viewPoint: Point): Option[Point] =
      (projectionMatrix × viewPoint.homogeneous).cartesian


  extension (point: Point)
    def toXY: PlanePoint = PlanePoint(point.x, point.y)
    
  trait WindowView[M]:

    // windowOrigin in Clip space where coordinates go from -1 to 1 on both Axises
    val windowOrigin: PlanePoint // Example/TopLeft = PlanePoint(-1, -1)
    val windowWidth: T
    val windowHeight: T

    val flipX: Boolean
    val flipY: Boolean

    protected val planeXSignum: T = if flipX then -__1 else __1
    protected val planeYSignum: T = if flipY then -__1 else __1

    val translation = AffineTranslation2D(-PlaneVector(windowOrigin))
    val scaling = Scaling2D(PlaneVector(planeXSignum * windowWidth / __2, planeYSignum * windowHeight / __2))
//    val baseChanging = Matrix2D.diagonal(planeYSignum, planeYSignum, __1)

//    val windowMatrix = scaling ×: translation ×: baseChanging
    val windowMatrix = scaling ×: translation 

    final def clipToWindow(clipPoint: Point): Option[PlanePoint] =
      (windowMatrix × clipPoint.toXY.homogeneous).cartesian
