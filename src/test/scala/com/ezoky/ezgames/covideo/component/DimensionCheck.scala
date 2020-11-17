/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension.{Geometry, PositionValue, SizeValue}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.delay
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

/**
 * @author gweinbach on 17/11/2020
 * @since 0.1.0
 */
class DimensionCheck extends Properties("Dimensions") {

  import Prop.forAll

  implicit lazy val GeometryArbitrary: Arbitrary[Geometry] =
    Arbitrary(Gen.oneOf(Geometry.Toric, Geometry.Bounded))

  implicit lazy val SizeArbitrary: Arbitrary[SizeValue] =
    Arbitrary[SizeValue](
      for {
        d <- arbitrary[Double]
      } yield {
        SizeValue(
          d
        )
      }
    )

  implicit lazy val PositionArbitrary: Arbitrary[PositionValue] =
    Arbitrary[PositionValue](
      for {
        d <- arbitrary[Double]
        size <- arbitrary[SizeValue]
        geometry <- arbitrary[Geometry]
      } yield {
        PositionValue(d, size, geometry)
      }
    )


  property("Position is always positive and within boundary limits") =
    forAll { (d: Double, boundary: SizeValue, geometry: Geometry) =>
      val position = PositionValue(d, boundary, geometry)
      !boundary.isOutOfBounds(position)
    }

  property("Zero sized boundary limits position to Zero") =
    forAll { (d: Double, geometry: Geometry) =>
      PositionValue(d, SizeValue.Zero, geometry) == PositionValue.Zero
    }

}
