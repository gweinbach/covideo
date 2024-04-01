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

  "Fractionals" should "be compared with precision" in {

    given Precision[Double] = Precision(1E-5)
    assert(0.0 ~= 0.00001)
    assert(!(0.0 ~= 0.0001))

    given Precision[Float] = Precision(1E-5)
    assert(0.0f ~= 0.00001f)
    assert(!(0.0f ~= 0.0001f))

    given Precision[BigDecimal] = Precision(BigDecimal(1, 255))
    assert(BigDecimal(0, 256) ~= BigDecimal(1, 256))
    assert(!(BigDecimal(0, 254) ~= BigDecimal(1, 254)))
  }

  "Fractional Epsilon" should "be something very small but not null" in {

    assert(1f + ε[Double] != 1f)
    given Precision[Float] = Precision(10f * ε[Float])
    assert(1f + ε[Float] ~= 1f)

    assert(1d + ε[Double] != 1d)
    given Precision[Double] = Precision(10d * ε[Double])
    assert(1d + ε[Double] ~= 1d)

    given Epsilon[BigDecimal] with
      override val value: BigDecimal = BigDecimal(1, 25)
    assert(BigDecimal(1, 0) + ε[BigDecimal] != BigDecimal(1, 0))
    given Precision[BigDecimal] = Precision(BigDecimal(1, 24))
    assert(BigDecimal(1, 0) + ε[BigDecimal] ~= BigDecimal(1, 0))
  }

  "Integral Epsilon" should "be null" in {
    assert(ε[Byte] === 0)
    assert(ε[Short] === 0)
    assert(ε[Int] === 0)
    assert(ε[Long] === 0L)
    assert(ε[BigInt] === BigInt(0))
  }

  "Fractional Epsilon" should "be seen as null if precision is of the same order" in {

    given Precision[Float] = Precision(ε[Float])
    given Precision[Double] = Precision(ε[Double])

    assert(ε[Float] ~= 0f)
    assert(ε[Double] ~= 0d)
  }
