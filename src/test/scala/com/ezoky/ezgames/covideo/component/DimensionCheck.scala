/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{delay, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Ordering.Implicits.*

import spire.*
import spire.math.*
import spire.implicits.*

/**
 * @author gweinbach on 17/11/2020
 * @since 0.1.0
 */
class DimensionCheck extends Properties("Dimensions") {

  import Prop.forAll

  implicit lazy val DurationArbitrary: Arbitrary[DurationValue] =
    Arbitrary(Gen.long.map(DurationValue(_)))

  implicit lazy val GeometryArbitrary: Arbitrary[Geometry] =
    Arbitrary(Gen.oneOf(Geometry.Flat, Geometry.Toric, Geometry.Bounded))

  lazy val NonFlatGeometryArbitrary: Arbitrary[Geometry] =
    Arbitrary(Gen.oneOf(Geometry.Toric, Geometry.Bounded))

  implicit lazy val SizeArbitrary: Arbitrary[SizeValue] =
    Arbitrary(Gen.double.map(SizeValue(_)))

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

  property("In a Flat Geometry, there is one single available Position") =
    forAll { (d: Double, boundary: SizeValue) =>
      PositionValue(d, boundary, Geometry.Flat) == PositionValue.Zero
    }

  property("Zero sized boundary limits position to Zero") =
    forAll { (d: Double, geometry: Geometry) =>
      PositionValue(d, SizeValue.Zero, geometry) == PositionValue.Zero
    }

  property("Position is always smaller than Maximum Position and greater or equal than Minimum") =
    forAll { (d: Double, boundary: SizeValue, geometry: Geometry) =>
      val position = PositionValue(d, boundary, geometry)
      given Geometry = geometry
      (boundary == SizeValue.Zero) || (geometry == Geometry.Flat) ||
        ((position >= boundary.minPosition) && (position < boundary.maxPosition))
    }

  property("Position is always within boundary limits") =
    forAll { (d: Double, boundary: SizeValue, geometry: Geometry) =>
      val position = PositionValue(d, boundary, geometry)
      boundary.isWithinBounds(position)
    }

  property("Min position is Zero") =
    forAll { (boundary: SizeValue, geometry: Geometry) =>
      given Geometry = geometry
      boundary.minPosition == PositionValue.Zero
    }
}
