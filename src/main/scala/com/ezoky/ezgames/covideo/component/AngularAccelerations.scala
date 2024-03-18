package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Generate.*

import spire.algebra.Trig
import spire.*
import spire.implicits.*

trait AngularAccelerations[T: Dimension: Trig]
  extends Spins[T]:

  import CoordsDimension.*
  
  case class AngularAcceleration(xAngularAcceleration: XAngularAcceleration,
                                 yAngularAcceleration: YAngularAcceleration,
                                 zAngularAcceleration: ZAngularAcceleration):

    def accelerate(spin: Spin,
                   within: SpinRange): Spin =
      Spin(
        xAngularAcceleration.accelerate(spin.xSpin, within),
        yAngularAcceleration.accelerate(spin.ySpin, within),
        zAngularAcceleration.accelerate(spin.zSpin, within)
      )

    def truncate(within: AngularAccelerationRange): AngularAcceleration =
      copy(
        xAngularAcceleration = xAngularAcceleration.truncate(within),
        yAngularAcceleration,
        zAngularAcceleration
      )

  trait AngularAccelerating[T]:
    extension (angularAccelerating: T) def angularAcceleration: AngularAcceleration


  object AngularAcceleration:

    val Zero: AngularAcceleration =
      AngularAcceleration(
        XAngularAcceleration.Zero,
        YAngularAcceleration.Zero,
        ZAngularAcceleration.Zero
      )

    def generated(xRange: AngularAccelerationRange,
                  yRange: AngularAccelerationRange,
                  zRange: AngularAccelerationRange): Generated[AngularAcceleration] =
      Generated.map3(
        XAngularAcceleration.generatedWithin(xRange),
        YAngularAcceleration.generatedWithin(yRange),
        ZAngularAcceleration.generatedWithin(zRange)
      )(AngularAcceleration(_, _, _))


  trait AngularAccelerationCoord[C <: Coord, S <: SpinCoord[C], A <: AngularAccelerationCoord[C, S, A]]:

    def value: AngularAccelerationValue

    inline protected def newSpin(spin: SpinValue): SpinValue =
      spin.angularAccelerate(value)

    def accelerate(spin: S,
                   within: SpinRange): S

    def truncate(within: AngularAccelerationRange): A


  /**
   * inclusive on both bounds
   */
  case class AngularAccelerationRange private(min: AngularAccelerationValue,
                                              max: AngularAccelerationValue):
    def generatedAngularAccelerationValue: Generated[AngularAccelerationValue] =
      AngularAccelerationValue.generatedBetween(min, max)

    def truncate(angularAccelerationValue: AngularAccelerationValue): AngularAccelerationValue =
      if (angularAccelerationValue < min)
        min
      else if (angularAccelerationValue > max)
        max
      else
        angularAccelerationValue


  object AngularAccelerationRange:

    val Null: AngularAccelerationRange =
      AngularAccelerationRange(AngularAccelerationValue.Zero, AngularAccelerationValue.Zero)

    def apply(min: AngularAccelerationValue,
              max: AngularAccelerationValue): AngularAccelerationRange =
      if (min <= max)
        new AngularAccelerationRange(min, max)

      else
        new AngularAccelerationRange(max, min)


  case class XAngularAcceleration(override val value: AngularAccelerationValue)
    extends AngularAccelerationCoord[XCoord, XSpin, XAngularAcceleration]:

    override def accelerate(spin: XSpin,
                            within: SpinRange): XSpin =
      XSpin(within.truncate(newSpin(spin.value)))

    override def truncate(within: AngularAccelerationRange): XAngularAcceleration =
      copy(value = within.truncate(value))

  object XAngularAcceleration:

    val Zero: XAngularAcceleration =
      XAngularAcceleration(AngularAccelerationValue.Zero)

    def generatedWithin(range: AngularAccelerationRange): Generated[XAngularAcceleration] =
      range.generatedAngularAccelerationValue.map(XAngularAcceleration(_))

  case class YAngularAcceleration(override val value: AngularAccelerationValue)
    extends AngularAccelerationCoord[YCoord, YSpin, YAngularAcceleration]:

    override def accelerate(spin: YSpin,
                            within: SpinRange): YSpin =
      YSpin(within.truncate(newSpin(spin.value)))

    override def truncate(within: AngularAccelerationRange): YAngularAcceleration =
      copy(value = within.truncate(value))

  object YAngularAcceleration:

    def Zero: YAngularAcceleration =
      YAngularAcceleration(AngularAccelerationValue.Zero)

    def generatedWithin(range: AngularAccelerationRange): Generated[YAngularAcceleration] =
      range.generatedAngularAccelerationValue.map(YAngularAcceleration(_))


  case class ZAngularAcceleration(override val value: AngularAccelerationValue)
    extends AngularAccelerationCoord[ZCoord, ZSpin, ZAngularAcceleration]:

    override def accelerate(spin: ZSpin,
                            within: SpinRange): ZSpin =
      ZSpin(within.truncate(newSpin(spin.value)))

    override def truncate(within: AngularAccelerationRange): ZAngularAcceleration =
      copy(value = within.truncate(value))

  object ZAngularAcceleration:

    def Zero: ZAngularAcceleration =
      ZAngularAcceleration(AngularAccelerationValue.Zero)

    def generatedWithin(range: AngularAccelerationRange): Generated[ZAngularAcceleration] =
      range.generatedAngularAccelerationValue.map(ZAngularAcceleration(_))
