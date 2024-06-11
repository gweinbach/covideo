package com.ezoky.ezgames.covideo.component

import spire.*
import spire.math.*

import scala.annotation.targetName


opaque type Rate = Double

object Rate:
  
  def apply(double: Double): Rate = double

  val Zero = Rate(0.0)

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

