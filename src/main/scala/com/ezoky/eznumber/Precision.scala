/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.eznumber

import spire.*
import spire.implicits.*
import spire.math.*

import scala.annotation.targetName

/**
 * @since 0.2.0
 * @author gweinbach on 06/06/2022
 */
trait Precisions[T: Numeric]:

  case class Precision(value: T)

  extension (t1: T)

    @targetName("equalsWithPrecision")
    infix def ~=(t2: T)
                (using precision: Precision): Boolean =
      (t1 - t2).abs <= precision.value