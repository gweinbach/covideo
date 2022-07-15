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
class Transformation3DTest extends AnyFlatSpec :

  given Precision[Double] = Precision(1E-10)

  val transformationMatrix = new Transformation3D[Double] {}

  import transformationMatrix.*

  "Points" can "be translated" in {
    val translation0 = Translation3D(SpaceVector.OneX)
    val translation1 = Translation3D(SpaceVector.OneZ + SpaceVector.OneY)

    assert(translation0 × SpacePoint.OneX.homogeneous === SpacePoint(2,0,0).homogeneous)
    assert(translation0.inverse × SpacePoint(2,0,0).homogeneous === SpacePoint.OneX.homogeneous)

    assert(translation1 × SpacePoint.OneX.homogeneous === SpacePoint(1,1,1).homogeneous)
    assert(translation1.inverse × SpacePoint(1,1,1).homogeneous === SpacePoint.OneX.homogeneous)
    assert(translation1.inverse × SpacePoint.OneX.homogeneous === SpacePoint(1, -1, -1).homogeneous)
  }

  "Normal Basis" can "be rotated" in {
    val rotatedBasis = Basis.orthonormalDirect(Axis.Y.base, Axis.Z.base).get
    val rotation = BasisTransformation(rotatedBasis)

    assert(rotation × SpacePoint.OneX.homogeneous === SpacePoint.OneY.homogeneous)
    assert(rotation × SpaceVector.OneX.homogeneous === SpaceVector.OneY.homogeneous)

    assert(rotation.inverse × SpacePoint.OneX.homogeneous === SpacePoint.OneZ.homogeneous)
    assert(rotation.inverse × SpaceVector.OneX.homogeneous === SpaceVector.OneZ.homogeneous)
  }

  "Point" can "be translated and rotated or the opposite" in {

    val rotatedBasis = Basis.orthonormalDirect(Axis.Y.base, Axis.Z.base).get
    val rotation = BasisTransformation(rotatedBasis)
    val translation = Translation3D(SpaceVector.OneZ + SpaceVector.OneY)

    assert((rotation ×: translation) × SpacePoint.OneX.homogeneous === SpacePoint(0, 2, 1).homogeneous)
    assert((translation.inverse ×: rotation.inverse) × SpacePoint(0, 2, 1).homogeneous === SpacePoint.OneX.homogeneous)
  }

  "Point rotation and translation combination" should "not be commutative" in {

    val rotatedBasis = Basis.orthonormalDirect(Axis.Y.base, Axis.Z.base).get

    val rotation = BasisTransformation(rotatedBasis)
    val translation = Translation3D(SpaceVector.OneZ + SpaceVector.OneY)

    assert((rotation ×: translation) × SpacePoint.OneX.homogeneous === SpacePoint(0, 2, 1).homogeneous)
    assert((translation ×: rotation) × SpacePoint.OneX.homogeneous === SpacePoint(1, 1, 1).homogeneous)
  }

  "A Coordinate System Transformation" should "be equivalent to the combination of a translation and a rotation" in {

    val rotatedBasis = Basis.orthonormalDirect(Axis.Y.base, Axis.Z.base).get

    val coordinateSystemTransformation = CoordinateSystemTransformation(SpacePoint(0, 1, 1), rotatedBasis)

    val rotation = BasisTransformation(rotatedBasis)
    val translation = Translation3D(SpaceVector.OneZ + SpaceVector.OneY)

    assert(coordinateSystemTransformation × SpacePoint.OneX.homogeneous === (rotation ×: translation) × SpacePoint.OneX.homogeneous)
    assert(coordinateSystemTransformation.inverse × SpacePoint.OneX.homogeneous === (rotation.inverse ×: translation.inverse) × SpacePoint.OneX.homogeneous)

    assert((rotation ×: translation) × SpacePoint.OneX.homogeneous === SpacePoint(0, 2, 1).homogeneous)
    assert((translation.inverse ×: rotation.inverse) × SpacePoint(0, 2, 1).homogeneous === SpacePoint.OneX.homogeneous)
  }
