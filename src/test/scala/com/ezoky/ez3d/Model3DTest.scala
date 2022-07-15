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

  "A Point in Space" can "be projected on a scene using a camera" in {

    val cameraDistance = 1
    val sceneWidth = 2
    val sceneHeight = 2
    val sceneDepth = 3

    val model3D = SimpleModel3D(sceneWidth, sceneHeight, cameraDistance)
    import model3D.{*, given}

    val camera: Camera =
      Perspective.LookAtCamera.viewBoxFromTop(sceneWidth, sceneHeight, sceneDepth, cameraDistance).get

    val window = Window(ScreenDimension(200 px, 100 px))

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

  "Every point in the View Frustum" should "be visible in the projection" in {

    val cameraDistance = 1500

    val sceneWidth = 1200
    val sceneHeight = 1000
    val sceneDepth = 800

    val model3D = SimpleModel3D(sceneWidth, sceneHeight, cameraDistance)
    import model3D.{*, given}

    val camera: Camera =
      Perspective.LookAtCamera.viewBoxFromTop(sceneWidth, sceneHeight, sceneDepth, cameraDistance).get

    val window = Window(ScreenDimension(1200 px, 1000 px))

    val pipeline3D = new Pipeline3D(camera, window)

    val componentPosition=SpacePoint(835.0271736662914,781.3540128758344,-164.91732501822892)
    val componentBasis =
      Basis.orthonormal(
        NonNullSpaceVector.safe(0.8122271271740444,-0.47947675976152787,0.33224859778663435).get,
        NonNullSpaceVector.safe(0.33224859778663635,0.8483945455193908,0.4121135333882649).get,
        NonNullSpaceVector.safe(-0.4794767597615279,-0.22434066056230295,0.848394545519392).get
      ).get
    val v=Vertex(SpacePoint(-20.0,-20.0,-20.0),SpacePoint(20.0,-20.0,-20.0))
    val shape = Shape(v)
    val componentTransformation = ComponentTransformation((componentPosition, componentBasis, shape))

    val worldV=Vertex(SpacePoint(821.7271943623084,778.4624703719232,-196.77245855211476),SpacePoint(854.2162794492701,759.2833999814621,-183.48251464064938))
    val cameraV=Vertex(SpacePoint(221.7271943623084,278.46247037192325,-1696.7724585521148),SpacePoint(254.21627944927013,259.28339998146214,-1683.4825146406495))
    val projectionV=Vertex(SpacePoint(0.32668964133162565,0.49233909172984824,-0.33318010262841086),SpacePoint(0.37751547348790593,0.4620482797888897,-0.3733083355433978))

    val flippedV=PlaneVertex(PlanePoint(0.32668964133162565,0.49233909172984824),PlanePoint(0.37751547348790593,0.4620482797888897))
    val planeV=PlaneVertex(PlanePoint(1.3266896413316256,1.4923390917298482),PlanePoint(1.377515473487906,1.4620482797888896))
    val windowV=PlaneVertex(PlanePoint(796.0137847989754,746.1695458649241),PlanePoint(826.5092840927437,731.0241398944448))

//    println(pipeline3D.viewMatrix(worldV).get)
//    println(pipeline3D.projectionMatrix(pipeline3D.viewMatrix(worldV).get).get)

    assert(componentTransformation.modelMatrix(v).get === worldV)

    assert(pipeline3D.viewMatrix(worldV).get === cameraV)
    assert(pipeline3D.projectionMatrix(cameraV).get === projectionV)
    assert(pipeline3D.windowFlip(projectionV).get === flippedV)
    assert(pipeline3D.windowTranslation(flippedV).get === planeV)
    assert(pipeline3D.windowScaling(planeV).get === windowV)

    val cameraMatrix =
      (pipeline3D.viewMatrix ×: pipeline3D.projectionMatrix)

    assert(cameraMatrix(worldV).get === projectionV)

    val clipMatrix =
      componentTransformation.modelMatrix ×: (pipeline3D.viewMatrix ×: pipeline3D.projectionMatrix)

    assert(clipMatrix(v).get === projectionV)

    val windowMatrix = pipeline3D.windowFlip  ×: (pipeline3D.windowTranslation ×: pipeline3D.windowScaling)

    assert(windowMatrix(projectionV).get === windowV)
  }

given Precision[Double] = Precision(1E-10)

class SimpleModel3D(sceneWidth: Double,
                    sceneHeight: Double,
                    cameraDistance: Double)
  extends Model3D[Double] with Cameras[Double]:

  given Model[Camera] with
    extension (camera: Camera)
      override def position: SpacePoint = camera.position
      override def basis: Basis = camera.basis

  given ProjectionView[Camera] with
    extension (camera: Camera)
      override def projectionMatrix: Matrix = camera.viewFrustum.projectionMatrix

  case class Window(dimension: ScreenDimension)

  given WindowView[Window] with
    extension (window: Window)
      def windowOrigin: PlanePoint = PlanePoint(-1, -1) // TopLeft = PlanePoint(-1, 1)
      def flipX: Boolean = false
      def flipY: Boolean = false
      def flipZ: Boolean = true
      def screenDimension: ScreenDimension = window.dimension

  // No rotation for a simple Basis
  given ComponentModel[Shape] with
    extension (model: Shape)
      override def position: SpacePoint = SpacePoint(sceneWidth/2, sceneHeight/2, 0)
      override def basis: Basis = Basis.NormalDirect
      override def shape: Shape = model

  given [B <: Basis]:ComponentModel[(SpacePoint, B, Shape)] with
    extension (model: (SpacePoint, B, Shape))
      override def position: SpacePoint = model._1
      override def basis: B = model._2
      override def shape: Shape = model._3
