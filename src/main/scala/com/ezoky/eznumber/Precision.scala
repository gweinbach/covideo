/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.eznumber

import scala.annotation.targetName

//import Numeric.Implicits._

import spire.*
import spire.math.*
import spire.implicits.*

/**
 * @since 0.2.0
 * @author gweinbach on 06/06/2022
 */
case class Precision[T: Numeric](value: T)

extension [T: Numeric](t1: T)

  @targetName("equalsWithPrecision")
  infix def ~=(t2: T)
              (using precision: Precision[T]): Boolean =
    (t1 - t2).abs <= precision.value