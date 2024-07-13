package com.ezoky.ezgames.covideo.component

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Prop, Properties}

import scala.math

class GenerateCheck extends Properties("Generate") {

  val generator = new Generate.RandomGenerator()

  property("Integral Range generation is left and right opened") =
    forAll { (min: BigInt, max: BigInt) =>
      val generated = Generate.generatedBetweenIntegral(min, max)
      val i = generated.get(generator)
      (i >= Integral[BigInt].min(min, max)) && (i <= Integral[BigInt].max(min, max))
    }

  property("Fractional Range generation is left and right opened") =
    forAll { (min: BigDecimal, max: BigDecimal) =>
      val generated = Generate.generatedBetweenFractional(min, max)
      val f = generated.get(generator)
      (f >= Fractional[BigDecimal].min(min, max)) && (f <= Fractional[BigDecimal].max(min, max))
    }
}
