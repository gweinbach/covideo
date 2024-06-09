package com.ezoky.ezgames.covideo.component

import scala.annotation.targetName

import spire.*
import spire.math.*
import spire.implicits.*


opaque type Rate = Double

extension (doubleRate: Double)
  @targetName("percent")
  inline def `%`: Rate =
    doubleRate / 100.0

  @targetName("perthousand")
  inline def `‰`: Rate =
    doubleRate / 1000.0

extension (rate: Rate)
  def apply[N: Numeric](n: N): N =
    val numeric = summon[Numeric[N]]
    numeric.fromDouble(numeric.toDouble(n) * rate)

//private trait RateT:
//  private[component] val _RateNumeric = summon[Numeric[Double]]
//
//given Numeric[Rate] = new RateT{}._RateNumeric

type DeathRate = Rate

type BirthRate = Rate

case class Demographies(birthRate: BirthRate = 0.0`‰`,
                        deathRate: DeathRate = 0.0`‰`)