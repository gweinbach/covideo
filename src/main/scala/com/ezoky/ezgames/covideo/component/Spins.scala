/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Generate.*

import spire.algebra.Trig

import spire.*
import spire.implicits.*
//import spire.math.{Quaternion as _, *}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Spins[T: Dimension: Trig]
  extends Coords[T]:

  import CoordsDimension.*
  import CoordsDimension.Ez3D.*

  case class Spin(xSpin: XSpin,
                  ySpin: YSpin,
                  zSpin: ZSpin):

    private lazy val optQuaternion =
      SpaceVector(
        xSpin.value.baseValue,
        ySpin.value.baseValue,
        zSpin.value.baseValue
      ).nonNull.map(Quaternion.fromRotationVector(_))

    def rotate(basis: Basis): Basis =
      (for
        quaternion <- optQuaternion
        rotatedBasis <-
          Basis.safe(
            Spin.rotate(quaternion, basis.i),
            Spin.rotate(quaternion, basis.j),
            Spin.rotate(quaternion, basis.k),
          )
      yield
        rotatedBasis).getOrElse(basis)

  trait Spinning[S]:
    extension (spinning: S)
      def spin: Spin

  object Spin:

    val Zero: Spin =
      Spin(
        XSpin.Zero,
        YSpin.Zero,
        ZSpin.Zero
      )

    def generated(xRange: SpinRange,
                  yRange: SpinRange,
                  zRange: SpinRange): Generated[Spin] =
      Generated.map3(
        XSpin.generatedWithin(xRange),
        YSpin.generatedWithin(yRange),
        ZSpin.generatedWithin(zRange)
      )(Spin(_, _, _))

    def rotate(quaternion: Quaternion,
               vector: NonNullSpaceVector): NonNullSpaceVector =
      quaternion.rotateNonNull(vector)



  trait SpinCoord[C <: Coord]:
    def value: SpinValue

  /**
   * inclusive on both bounds
   */
  case class SpinRange private(min: SpinValue,
                               max: SpinValue):
    def generatedSpinValue: Generated[SpinValue] =
      SpinValue.generatedBetween(min, max)

    def truncate(spinValue: SpinValue): SpinValue =
      if (spinValue < min)
        min
      else if (spinValue > max)
        max
      else
        spinValue

  object SpinRange:

    val Null: SpinRange =
      SpinRange(SpinValue.Zero, SpinValue.Zero)

    def apply(min: SpinValue,
              max: SpinValue): SpinRange =
      if (min <= max) {
        new SpinRange(min, max)
      }
      else {
        new SpinRange(max, min)
      }


  case class XSpin(override val value: SpinValue)
    extends SpinCoord[XCoord]


  object XSpin:

    val Zero: XSpin =
      XSpin(SpinValue.Zero)

    def generatedWithin(range: SpinRange): Generated[XSpin] =
      range.generatedSpinValue.map(XSpin(_))

  case class YSpin(override val value: SpinValue)
    extends SpinCoord[YCoord]

  object YSpin:

    def Zero: YSpin =
      YSpin(SpinValue.Zero)

    def generatedWithin(range: SpinRange): Generated[YSpin] =
      range.generatedSpinValue.map(YSpin(_))


  case class ZSpin(override val value: SpinValue)
    extends SpinCoord[ZCoord]

  object ZSpin:

    def Zero: ZSpin =
      ZSpin(SpinValue.Zero)

    def generatedWithin(range: SpinRange): Generated[ZSpin] =
      range.generatedSpinValue.map(ZSpin(_))
