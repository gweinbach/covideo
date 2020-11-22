/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.delay
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

/**
 * @author gweinbach on 17/11/2020
 * @since 0.1.0
 */
class DimensionCheck extends Properties("Dimensions") {

  import Prop.forAll

  given Arbitrary[Geometry] =
    Arbitrary(Gen.oneOf(Geometry.Toric, Geometry.Bounded))

  given Arbitrary[SizeValue] =
    Arbitrary[SizeValue](arbitrary[Double].map(SizeValue(_)))

  given Arbitrary[PositionValue] =
    Arbitrary[PositionValue](
      for {
        d <- arbitrary[Double]
        size <- arbitrary[SizeValue]
        geometry <- arbitrary[Geometry]
      } yield {
        PositionValue(d, size, geometry)
      }
    )

  given Arbitrary[MovementValue] =
    Arbitrary[MovementValue](arbitrary[Double].map(MovementValue(_)))
  
  

  property("Position is always positive and within boundary limits") =
    forAll { (d: Double, boundary: SizeValue, geometry: Geometry) =>
      val position = PositionValue(d, boundary, geometry)
      !boundary.isOutOfBounds(position)
    }

  property("Zero sized boundary limits position to Zero") =
    forAll { (d: Double, geometry: Geometry) =>
      PositionValue(d, SizeValue.Zero, geometry) == PositionValue.Zero
    }
  
  property("applying a movement M to A gives a position B implies that M is the movement from A to B") =
    forAll { (da: Double, db: Double, boundary: SizeValue, geometry: Geometry) =>
      given SizeValue = boundary
      given Geometry = geometry
      
      val a = da position
      val b = (db position) // compilation fails without parenthesis
      MovementValue(a, b) == a.to(b)
    }

}
