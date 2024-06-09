package com.ezoky.ezgames.covideo.component

import org.scalatest.flatspec.AnyFlatSpec

import spire.*
import spire.math.*
import spire.implicits.*

class RateTest extends AnyFlatSpec:

  "% or ‰ operators" should "be used to define rates" in {
    assert((1.0`‰`) === 0.001)
//    assert((1.0`%`) === 0.01)
  }

  "rate" can "be applied to any number type" in {
    assert(50`‰`(100) === 5)
    assert(50`‰`(100.0) === 5.0)
  }

  "rate" can "be summed" in {
    assert(((50 `‰`) + (50 `‰`))(100.0) === 10.0)
  }

