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
class TransformationMatrixTest extends AnyFlatSpec:

  given Precision[Double] = Precision(1E-16)
  val transformationMatrix = new TransformationMatrix[Double] {}
  import transformationMatrix.*

  "Normal Basis" can "be rotated" in {
    val rotatedBasis = Basis.orthonormal(Axis.Y.base, Axis.Z.base).get
    val rotation = BasisRotation(rotatedBasis).inverse

    assert((rotation × Point.OneX.homogeneous) === Point.OneY.homogeneous)
  }
