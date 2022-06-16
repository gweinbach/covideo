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

  given Precision[Double] = Precision(1E-16)
  val space = new Space[Double] {}
  import space.*

  "Normal Basis" should "be orthogonal and normalized" in {
    val normalBasis = Basis.Normal
    assert(normalBasis.isNormalized)
    assert(normalBasis.isOrthogonal)
  }

  "Normal Basis" can "be built from 2 base vectors" in {
    val normalBasis = Basis.orthonormal(Axis.X.base, Axis.Y.base)
    assert(normalBasis === Some(Basis.Normal))
  }