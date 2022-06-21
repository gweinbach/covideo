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

  "Normal Basis" can "be rotated" in {
    val rotatedBasis = Basis.orthonormal(Axis.Y.base, Axis.Z.base).get
    val rotation = BasisTransformation(rotatedBasis)

    assert(rotation × SpacePoint.OneX.homogeneous === SpacePoint.OneZ.homogeneous)
    assert(rotation × Vector.OneX.homogeneous === Vector.OneZ.homogeneous)

    assert(rotation.inverse × SpacePoint.OneX.homogeneous === SpacePoint.OneY.homogeneous)
    assert(rotation.inverse × Vector.OneX.homogeneous === Vector.OneY.homogeneous)
  }

  "Point" can "be translated" in {

    val translation = AffineTranslation(Vector.OneZ + Vector.OneY)

    assert(translation × SpacePoint.OneX.homogeneous === SpacePoint(1, 1, 1).homogeneous)
    assert(translation.inverse × SpacePoint.OneX.homogeneous === SpacePoint(1, -1, -1).homogeneous)
  }

  "Point" can "be translated and rotated or the opposite" in {

    val rotatedBasis = Basis.orthonormal(Axis.Y.base, Axis.Z.base).get
    val rotation = BasisTransformation(rotatedBasis)
    val translation = AffineTranslation(Vector.OneZ + Vector.OneY)

    assert((translation ×: rotation) × SpacePoint.OneX.homogeneous === SpacePoint(1, 1, 1).homogeneous)
    assert((rotation ×: translation) × SpacePoint.OneX.homogeneous === SpacePoint(0, 1, 2).homogeneous)
  }
