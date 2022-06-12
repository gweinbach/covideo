/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.eznumber

import org.scalatest.flatspec.AnyFlatSpec

import spire.*
import spire.math.*
import spire.algebra.Trig
import spire.implicits.*


/**
 * @since 0.2.0
 * @author gweinbach on 12/06/2022
 */
class AngleTest extends AnyFlatSpec:

  val PI: Double = math.pi

  val Angles = new Angles[Double] {}
  import Angles.*

  "Trigonometric functions" should "give same result whether you use degrees or radians" in {

    val piRad = (PI radians)
    assert(-1.0 === cos(piRad))

    val _180Deg = (180 degrees)
    assert(-1.0 === cos(_180Deg))

    val xRad: Radians = acos(-1.0)
    assert(piRad === xRad)

    val xDeg: Degrees = acos(-1.0)
    assert(_180Deg === xDeg)
  }

  "Angles" can "be compared with precision" in {

    given Numeric[Radians] = RadianIsNumeric
    given Precision[Radians] = Precision(1E-10 radians)

    assert((PI radians) ~= (3.1415926535 radians))
    assert((PI radians) ~= (3.14159265355555 radians))
    assert(!((PI radians) ~= (3.1415926534 radians)))


    given Numeric[Degrees] = DegreeIsNumeric
    given Precision[Degrees] = Precision(1E-2 degrees)

    assert((180 degrees) ~= (180.01 degrees))
    assert(!((180 degrees) ~= (180.1 degrees)))
  }
