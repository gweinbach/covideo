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

    val componentPosition=SpacePoint(1029.3867964965905,646.923968830763,698.354018760909)
    val componentBasis: Basis = Basis.orthonormal(
        NonNullSpaceVector.safe(0.4311391013614713,-0.9017784842957555,0.030243024625077768).get,
        NonNullSpaceVector.safe(0.030243024625067245,0.047942315902163725,0.9983921543198669).get
      ).get
    val v=Vertex(SpacePoint(-20.0,-20.0,-20.0),SpacePoint(20.0,-20.0,-20.0))
    val shape = Shape(v)
    val componentTransformation = ComponentTransformation((componentPosition, componentBasis, shape))

    val worldV=Vertex(SpacePoint(1038.1947236627748,672.5913173122616,676.8224688639667),SpacePoint(1055.4402877172336,636.5201779404314,678.0321898489699))
    val cameraV=Vertex(SpacePoint(438.1947236627748,172.5913173122616,-823.1775311360333),SpacePoint(455.44028771723356,136.52017794043138,-821.9678101510301))
    val projectionV=Vertex(SpacePoint(1.3308026127062784,0.628994272015936,-5.727691231558512),SpacePoint(1.3852132714100749,0.49826833698760126,-5.7431116443784145))

    val flippedV=PlaneVertex(PlanePoint(1.3308026127062784,0.628994272015936),PlanePoint(1.3852132714100749,0.49826833698760126))
    val planeV=PlaneVertex(PlanePoint(2.3308026127062784,1.628994272015936),PlanePoint(2.385213271410075,1.4982683369876013))
    val windowV = PlaneVertex(PlanePoint(1398.481567623767,814.497136007968),PlanePoint(1431.127962846045,749.1341684938006))

    assert(componentTransformation.modelMatrix(v).get === worldV)

    assert(pipeline3D.viewMatrix(worldV).get === cameraV)
    assert(pipeline3D.projectionMatrix(cameraV).get === projectionV)
    assert(pipeline3D.windowFlip(projectionV).get === flippedV)
    assert(pipeline3D.windowTranslation(flippedV).get === planeV)
    assert(pipeline3D.windowScaling(planeV).get === windowV)
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
      def screenDimension: ScreenDimension = window.dimension

  // No rotation for a simple Basis
  given ComponentModel[Shape] with
    extension (model: Shape)
      override def position: SpacePoint = SpacePoint(sceneWidth/2, sceneHeight/2, 0)
      override def basis: Basis = Basis.Normal
      override def shape: Shape = model

  given ComponentModel[(SpacePoint, Basis, Shape)] with
    extension (model: (SpacePoint, Basis, Shape))
      override def position: SpacePoint = model._1
      override def basis: Basis = model._2
      override def shape: Shape = model._3
