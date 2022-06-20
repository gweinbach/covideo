/*
 * @author gweinbach on 18/06/2022 22:38
 * @since 0.2.0
 */

package com.ezoky.ez3d

import com.ezoky.ez3d.Screen.*
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
    with Plane[T] with Transformation2D[T] :

  private val _Numeric = summon[Numeric[T]]
  private val _0 = _Numeric.zero
  private val __1 = _Numeric.one // double '_' to avoid conflict with Product<X>._1
  private val __2 = _Numeric.fromInt(2) // double '_' to avoid conflict with Product<X>._1

  trait Model[M]:
    extension (modeled: M)
      def position: SpacePoint
      def basis: Basis

  /**
   * For test purposes
   */
  final def shapeToX(pointToPoint: SpacePoint => Option[SpacePoint]): Shape => Shape =
    shape =>
      Shape(
        shape.vertices.flatMap(vertex =>
          for
            s <- pointToPoint(vertex.s)
            t <- pointToPoint(vertex.t)
          yield
            Vertex(s, t))
      )

  trait ModelTransformation[M: Model](modeled: M):

    private val _translation = AffineTranslation(Vector(modeled.position))
    private val _orientation = BasisTransformation(modeled.basis)

    // They are lazy because only one minght be useful depending on the usage of the view (for camera or for component)
//    final lazy val modelMatrix: Matrix = _translation ×: _orientation
//    final lazy val viewMatrix: Matrix = _orientation.inverse ×: _translation.inverse
    final lazy val modelMatrix: Matrix = _translation ×: _orientation
    final lazy val viewMatrix: Matrix = _orientation.inverse ×: _translation.inverse

    final def modelToWorld(modelPoint: SpacePoint): Option[SpacePoint] =
      (modelMatrix × modelPoint.homogeneous).cartesian

    final def worldToView(worldPoint: SpacePoint): Option[SpacePoint] =
      (viewMatrix × worldPoint.homogeneous).cartesian

    /**
     * For test purposes
     */
    final def shapeToWorld(shape: Shape): Shape =
      shapeToX(modelToWorld)(shape)

    /**
     * For test purposes
     */
    final def shapeToView(shape: Shape): Shape =
      shapeToX(worldToView)(shape)


  trait ProjectionView[M]:
    extension (projected: M)
      def projectionMatrix: Matrix

  trait ProjectionViewTransformation[M: ProjectionView](view: M):

    final val projectionMatrix = view.projectionMatrix

    final def viewToClip(viewPoint: SpacePoint): Option[SpacePoint] =
      (projectionMatrix × viewPoint.homogeneous).cartesian

    /**
     * For test purposes
     */
    final def shapeToClip(shape: Shape): Shape =
      shapeToX(viewToClip)(shape)

  extension (point: SpacePoint)
    def toXY: PlanePoint = PlanePoint(point.x, point.y)

  trait WindowView[M]:
    extension (windowed: M)
    // windowOrigin in Clip space where coordinates go from -1 to 1 on both Axises
      def windowOrigin: PlanePoint // Example: TopLeft = PlanePoint(-1, -1)
      def flipX: Boolean
      def flipY: Boolean
      def screenDimension: ScreenDimension

  trait WindowViewTransformation[M: WindowView](windowed: M):

    private val windowWidth: T = windowed.screenDimension.width
    private val windowHeight: T = windowed.screenDimension.height

    private val planeXSignum: T = if windowed.flipX then -__1 else __1
    private val planeYSignum: T = if windowed.flipY then -__1 else __1

    private val translation = AffineTranslation2D(-PlaneVector(windowed.windowOrigin))
    private val scaling = Scaling2D(PlaneVector(planeXSignum * windowWidth , planeYSignum * windowHeight ))
    //   private val baseChanging = Matrix2D.diagonal(planeYSignum, planeYSignum, __1)

    final val windowMatrix = scaling ×: translation
    //   final val windowMatrix = scaling ×: translation ×: baseChanging

    final def clipToWindow(clipPoint: SpacePoint): Option[ScreenPosition] =
      (windowMatrix × clipPoint.toXY.homogeneous).cartesian.map(point => ScreenPosition(point.x px, point.y px))


    /**
     * For test purposes
     */
    final def shapeToWindow(shape: Shape): ScreenShape =
      ScreenShape(
        shape.vertices.flatMap(vertex =>
          for
            s <- clipToWindow(vertex.s)
            t <- clipToWindow(vertex.t)
          yield
            ScreenVertex(s, t))
      )


  trait ComponentModel[M]
    extends Model[M] :
    extension (component: M)
      def shape: Shape

  trait ComponentTransformation[M: ComponentModel](component: M)
    extends ModelTransformation[M] :

    final val shape = component.shape


  class Pipeline3D[C: Model : ProjectionView, W: WindowView](camera: C,
                                                             window: W)
    extends ModelTransformation[C](camera)
      with ProjectionViewTransformation[C](camera)
      with WindowViewTransformation[W](window) :

    val cameraMatrix = projectionMatrix ×: viewMatrix

    def run[M: ComponentModel](component: M): ScreenShape =
      // specific to M
      val componentTransformation = new ComponentTransformation(component) with ModelTransformation(component) {}
      val modelModelMatrix = componentTransformation.modelMatrix
      val shape = componentTransformation.shape

      val clippingMatrix = (projectionMatrix ×: (viewMatrix ×: modelModelMatrix))

      val screenVertices = shape.vertices.map(v => transformVertex(clippingMatrix, v))

      ScreenShape(
        // TODO: take care of potential tansformation problemns resulting in None vertices
        screenVertices.flatten
      )

    private def transformVertex(clippingMatrix: Matrix,
                                vertex: Vertex): Option[ScreenVertex] =
      for
        sourcePosition <- transformPoint(clippingMatrix, vertex.s)
        targetPosition <- transformPoint(clippingMatrix, vertex.t)
      yield
        ScreenVertex(sourcePosition, targetPosition)

    private def transformPoint(clippingMatrix: Matrix,
                               point: SpacePoint): Option[ScreenPosition] =
      for
        clipPoint <- (clippingMatrix × point.homogeneous).cartesian
        windowPoint <- (windowMatrix × clipPoint.toXY.homogeneous).cartesian
      yield
        ScreenPosition(windowPoint.x px, windowPoint.y px)

