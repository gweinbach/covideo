/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 */

package com.ezoky.eznumber

import org.scalatest.flatspec.AnyFlatSpec

import spire.*
import spire.math.*
import spire.implicits.*

/**
 * @since 0.2.0
 * @author gweinbach on 12/06/2022
 */
class PrecisionTest extends AnyFlatSpec:

  "Double" should "be compared with precision" in {

    given Precision[Double] = Precision(1E-5)

    assert(0.0 ~= 0.00001)
    assert(!(0.0 ~= 0.0001))
  }

  "Epsilon" should "be something very small but not null" in {

    assert(1f + ε[Float] != 1f)
    assert(1d + ε[Double] != 1d)

    given Precision[Float] = Precision(10f * ε[Float])
    given Precision[Double] = Precision(10d * ε[Double])

    assert(1f + ε[Float] ~= 1f)
    assert(1d + ε[Double] ~= 1d)
  }
