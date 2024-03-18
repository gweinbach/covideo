package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension
import com.ezoky.eznumber.{Precision, given}
import com.ezoky.ez3d.given 

import spire.*
import spire.implicits.*
import spire.math.*

package object double:

  given Precision[Double] = Precision(1E-10d)

  object DoubleDimension extends Dimension[Double]:

    final override def modulo(a: Double, 
                        b: Double): Double = a % b

    final override protected def _NumberToDimensionConverter[N: Numeric]: (N) => Double =
      (n: N) => summon[Numeric[N]].toDouble(n)
