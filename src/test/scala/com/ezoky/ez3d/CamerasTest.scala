/*
 * @author gweinbach on 16/06/2022 21:03
 * @since 0.2.0
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.Precision
import org.scalatest.flatspec.AnyFlatSpec
import spire.*
import spire.algebra.Trig
import spire.implicits.*
import spire.math.*

/**
 * @since 0.2.0
 * @author gweinbach on 16/06/2022
 */
class CamerasTest extends AnyFlatSpec :

  given Precision[Double] = Precision(1E-10)

  val projection3D = new Cameras[Double] {}

  import projection3D.*

  "View Frustrum" can "define plane projection of a volume of space" in {
    //    val perspectiveViewFrustum =
    //      Perspective.ViewFrustum.fromFieldOfView(10.0, 50.0, 1.0, 90.0 degrees).get
    val perspectiveViewFrustum =
      Perspective.ViewFrustum.fromSymetricPlanes(20, 220, 300, 400).get
    val projection = perspectiveViewFrustum.projectionMatrix

    // assert(perspectiveViewFrustum.alternateProjectionMatrix === projection)

    //    val testPoints = List[Object3D](
    //      perspectiveViewFrustum.nearBottomLeft,
    //      perspectiveViewFrustum.nearTopLeft,
    //      perspectiveViewFrustum.nearBottomRight,
    //      perspectiveViewFrustum.nearTopRight,
    //
    //      Point(0,0,0),
    //
    //      perspectiveViewFrustum.farBottomLeft,
    //      perspectiveViewFrustum.farTopLeft,
    //      perspectiveViewFrustum.farBottomRight,
    //      perspectiveViewFrustum.farTopRight,
    //
    //      Point(0,0,0),
    //
    //      perspectiveViewFrustum.center,
    //
    //      Point(0,0,0),
    //
    //      Point(0, 0, perspectiveViewFrustum.middleZ +13.33333333333333),
    //
    //    )
    //    testPoints.foreach {
    //      point =>
    //        val proj = projection × point.homogeneous
    //        println(s"$point\t->${proj}\t->${proj.cartesian.fold(None)(p => p)}")
    //    }

    assert((projection × perspectiveViewFrustum.nearBottomLeft.homogeneous).cartesian === Some(SpacePoint(-1,-1,-1)))
    assert((projection × perspectiveViewFrustum.nearTopLeft.homogeneous).cartesian === Some(SpacePoint(-1,1,-1)))
    assert((projection × perspectiveViewFrustum.nearBottomRight.homogeneous).cartesian === Some(SpacePoint(1,-1,-1)))
    assert((projection × perspectiveViewFrustum.nearTopRight.homogeneous).cartesian === Some(SpacePoint(1,1,-1)))

    assert((projection × perspectiveViewFrustum.farBottomLeft.homogeneous).cartesian === Some(SpacePoint(-1,-1,1)))
    assert((projection × perspectiveViewFrustum.farTopLeft.homogeneous).cartesian === Some(SpacePoint(-1,1,1)))
    assert((projection × perspectiveViewFrustum.farBottomRight.homogeneous).cartesian === Some(SpacePoint(1,-1,1)))
    assert((projection × perspectiveViewFrustum.farTopRight.homogeneous).cartesian === Some(SpacePoint(1,1,1)))

    assert((projection × perspectiveViewFrustum.center.homogeneous).cartesian === Some(SpacePoint(0, 0, 0)))
  }