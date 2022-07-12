/*
 * @author gweinbach on 16/06/2022 21:03
 * @since 0.2.0
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.Precision
import org.scalatest.flatspec.AnyFlatSpec
import spire.*
import spire.implicits.*
import spire.math.*

/**
 * @since 0.2.0
 * @author gweinbach on 16/06/2022
 */
class Transformation2DTest extends AnyFlatSpec :

  given Precision[Double] = Precision(1E-10)

  val transformationMatrix = new Transformation2D[Double] {}

  import transformationMatrix.*

  "Points" can "be translated" in {
    val translation0 = Translation2D(PlaneVector.OneX)
    val translation1 = Translation2D(PlaneVector.OneX + PlaneVector.OneY)

    assert(translation0 × PlanePoint.OneX.homogeneous === PlanePoint(2,0 ).homogeneous)
    assert(translation0.inverse × PlanePoint(2, 0).homogeneous === PlanePoint.OneX.homogeneous)

    assert(translation1 × PlanePoint.OneX.homogeneous === PlanePoint(2, 1).homogeneous)
    assert(translation1.inverse × PlanePoint(2, 1).homogeneous === PlanePoint.OneX.homogeneous)
    assert(translation1.inverse × PlanePoint.OneX.homogeneous === PlanePoint(0, -1).homogeneous)
  }

  "Normal Basis" can "be rotated" in {
    val rotatedBasis = Basis2D.orthonormal(NonNullPlaneVector.OneY)
    val rotation = BasisTransformation2D(rotatedBasis)

    assert((rotation × PlanePoint.OneX.homogeneous).cartesian.get === PlanePoint(0, -1))
    assert((rotation × PlaneVector.OneX.homogeneous) === PlaneVector(0, -1).homogeneous)

    assert((rotation.inverse × PlanePoint.OneY.homogeneous).cartesian.get === PlanePoint(-1, 0))
    assert(rotation.inverse × PlaneVector.OneY.homogeneous === PlaneVector(-1, 0).homogeneous)
  }

  "Point" can "be translated and rotated or the opposite" in {

    val rotatedBasis = Basis2D.orthonormal(-NonNullPlaneVector.OneY)
    val rotation = BasisTransformation2D(rotatedBasis)
    val translation = Translation2D(PlaneVector.OneX + PlaneVector.OneY)

    assert((rotation ×: translation) × PlanePoint.OneX.homogeneous === PlanePoint(1, 2).homogeneous)
    assert((translation.inverse ×: rotation.inverse) × PlanePoint(1, 2).homogeneous === PlanePoint.OneX.homogeneous)
  }

//  "Point rotation and translation combination" should "not be commutative" in {
//
//    val rotatedBasis = Basis2D.orthonormal(NonNullPlaneVector.OneY).get
//
//    val rotation = BasisTransformation2D(rotatedBasis)
//    val translation = Translation2D(PlaneVector.OneX + PlaneVector.OneY)
//
//    assert((rotation ×: translation) × PlanePoint.OneX.homogeneous === PlanePoint(1, 2).homogeneous)
//    assert((translation ×: rotation) × PlanePoint.OneX.homogeneous === PlanePoint(1, 1, 1).homogeneous)
//  }

//  "A Coordinate System Transformation" should "be equivalent to the combination of a translation and a rotation" in {
//
//    val rotatedBasis = Basis2D.orthonormal(NonNullPlaneVector.OneY).get
//
//    val coordinateSystemTransformation = CoordinateSystemTransformation(PlanePoint(0, 1, 1), rotatedBasis)
//
//    val rotation = BasisTransformation2D(rotatedBasis)
//    val translation = Translation2D(PlaneVector.OneX + PlaneVector.OneY)
//
//    assert(coordinateSystemTransformation × PlanePoint.OneX.homogeneous === (rotation ×: translation) × PlanePoint.OneX.homogeneous)
//    assert(coordinateSystemTransformation.inverse × PlanePoint.OneX.homogeneous === (rotation.inverse ×: translation.inverse) × PlanePoint.OneX.homogeneous)
//
//    assert((rotation ×: translation) × PlanePoint.OneX.homogeneous === PlanePoint(0, 2, 1).homogeneous)
//    assert((translation.inverse ×: rotation.inverse) × PlanePoint(0, 2, 1).homogeneous === PlanePoint.OneX.homogeneous)
//  }
