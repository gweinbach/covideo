package com.ezoky.ezgames.covideo.component

import com.ezoky.ez3d.given
import com.ezoky.eznumber.{Precision, given}
import spire.*
import spire.implicits.*
import spire.math.*

package object float:

  given Precision[Float] = Precision(1E-6f)

  object FloatDimension extends Dimension[Float]:

    final override def modulo(a: Float,
                              b: Float): Float = a % b

    final override protected def _NumberToDimensionConverter[N: Numeric]: (N) => Float =
      (n: N) => summon[Numeric[N]].toFloat(n)
