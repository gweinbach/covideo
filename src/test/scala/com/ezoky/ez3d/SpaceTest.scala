/*
 * @author gweinbach on 16/06/2022 21:03
 * @since 0.2.0
 */

package com.ezoky.ez3d

import org.scalatest.flatspec.AnyFlatSpec

import spire.*
import spire.math.*
import spire.implicits.*
import com.ezoky.eznumber.Precision

/**
 * @since 0.2.0
 * @author gweinbach on 16/06/2022
 */
class SpaceTest extends AnyFlatSpec:

  given Precision[Double] = Precision(1E-10)
  val space = new Space[Double] {}
  import space.*

  "Normal Basis" should "be orthogonal and normalized" in {
    val normalBasis = Basis.NormalDirect
    assert(normalBasis.isNormalized)
    assert(normalBasis.isOrthogonal)
  }

  "Normal Basis" can "be built from 2 base vectors" in {
    val normalBasis = Basis.orthonormalDirect(Axis.X.base, Axis.Y.base)
    assert(normalBasis === Some(Basis.NormalDirect))
  }

  "Normal Basis" can "be built from any 2 orthogonal vectors" in {

    val vectorI = NonNullSpaceVector.safe(0.9993743358189562, 0.020876275287366058, -0.028550272092336897).get
    val vectorJ = NonNullSpaceVector.safe(-0.028550272092343565, 0.952624998820267, -0.3028043817155551).get
    val vectorK = NonNullSpaceVector.safe(0.020876275287364396, 0.3034300458965983, 0.9526249988202674).get

    val expectedComponentBasis =
      Basis.safe(vectorI, vectorJ, vectorK).get

    val componentBasisIJ: Basis =
      Basis.orthonormalDirect(vectorI, vectorJ).get

    assert(componentBasisIJ === expectedComponentBasis)
    assert(componentBasisIJ.k === vectorK)
    assert(componentBasisIJ.isNormalized)
    assert(componentBasisIJ.isOrthogonal)

    val componentBasisJK: Basis =
      Basis.orthonormalDirect(vectorJ, vectorK).get

    assert(componentBasisJK.k === vectorI)

    val componentBasisKI: Basis =
      Basis.orthonormalDirect(vectorK, vectorI).get

    assert(componentBasisKI.k === vectorJ)

    val componentBasisJI: Basis =
      Basis.orthonormalDirect(vectorJ, vectorI).get

    assert(componentBasisJI.k === (-vectorK))
    assert(componentBasisJI.isNormalized)
    assert(componentBasisJI.isOrthogonal)

  }