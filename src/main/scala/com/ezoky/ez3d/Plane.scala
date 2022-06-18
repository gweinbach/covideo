/*
 * @author gweinbach on 15/06/2022 22:46
 * @since 0.2.0
 */

package com.ezoky.ez3d

import spire.*
import spire.math.*
import spire.implicits.*

/**
 * @since $NEXT_VERSION
 * @author gweinbach on 15/06/2022
 */
trait Plane[T: Numeric]:

  case class PlanePoint(x: T, 
                        y: T)

  case class PlaneVector(x: T,
                         y: T)
  
  object PlaneVector:

    def apply(p1: PlanePoint, 
              p2: PlanePoint): PlaneVector = 
      new PlaneVector(
        p2.x - p1.x,
        p2.y - p1.y
      )
  