/*
 * @author gweinbach on 21/06/2022 23:09
 * @since 0.2.0
 */

package com.ezoky.ez3d


import com.ezoky.ez3d.Screen.*
import com.ezoky.eznumber.{Angles, Precision}
import spire.*
import spire.algebra.Trig
import spire.implicits.*
import spire.math.*
import org.scalatest.flatspec.AnyFlatSpec

/**
 * @since 0.2.0
 * @author gweinbach on 21/06/2022
 */
class Model3DTest
  extends AnyFlatSpec :

  given Precision[Double] = Precision(1E-10)

  val model3D = new Model3D[Double] with Cameras[Double] {}
  import model3D.{*, given}

  "A Point in Space" can "be projected on a scene using a camera" in {

    val viewFrustum =
      Perspective.ViewFrustum.fromSymetricPlanes(
        nearDistance = 1,
        farDistance = 4,
        topDistance = 1,
        rightDistance = 1
      ).get
    val camera: Camera = Perspective.LookAtCamera.safe(
      position = SpacePoint(1,1,1),
      target = SpacePoint(1,1,0),
      upVector = SpaceVector.OneY,
      viewFrustum
    ).get


    given Model[Camera] with
      extension (camera: Camera)
        override def position: SpacePoint = camera.position
        override def basis: Basis = camera.basis

    given ProjectionView[Camera] with
      extension (camera: Camera)
        override def projectionMatrix: Matrix = camera.viewFrustum.projectionMatrix


    case class Window(dimension: ScreenDimension)
    val window = Window(ScreenDimension(200 px, 100 px))

    given WindowView[Window] with
      extension (scene: Window)
        def windowOrigin: PlanePoint = PlanePoint(-1, -1) // TopLeft = PlanePoint(-1, 1)
        def flipX: Boolean = false
        def flipY: Boolean = false
        def screenDimension: ScreenDimension = window.dimension

    given ComponentModel[Shape] with
      extension (model: Shape)
        override def position: SpacePoint = SpacePoint(1, 1, 0)
        override def basis: Basis = camera.basis
        override def shape: Shape = model

    val pipeline3D = new Pipeline3D(camera, window)

    val vertexCenter = Vertex(SpacePoint(0, 0, 0), SpacePoint(0, 0, -1))
    val simpleShape = Shape(vertexCenter)

    assert(pipeline3D.run(simpleShape) === ScreenShape(ScreenVertex(ScreenPosition(100 px, 50px), ScreenPosition(100 px, 50px))))

    val diagonalShape =
      Shape(
        Vertex(SpacePoint(-1, -1, 0), SpacePoint(0, 0, -1)),
        Vertex(SpacePoint(-1, 1, 0), SpacePoint(0, 0, -1)),
        Vertex(SpacePoint(1, -1, 0), SpacePoint(0, 0, -1)),
        Vertex(SpacePoint(1, 1, 0), SpacePoint(0, 0, -1))
      )
    val screenDiagonalShape =
      ScreenShape(
        ScreenVertex(ScreenPosition(0px, 0 px), ScreenPosition(100 px, 50px)),
        ScreenVertex(ScreenPosition(0 px, 100px), ScreenPosition(100 px, 50px)),
        ScreenVertex(ScreenPosition(200 px, 0 px), ScreenPosition(100 px, 50px)),
        ScreenVertex(ScreenPosition(200 px, 100 px), ScreenPosition(100 px, 50px))
      )

    assert(pipeline3D.run(diagonalShape) === screenDiagonalShape)
  }