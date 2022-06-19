/*
 * @author gweinbach on 15/06/2022 22:46
 * @since 0.2.0
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.*
import com.sun.management.VMOption.Origin
import spire.*
import spire.implicits.*
import spire.math.*

/**
 * @since $NEXT_VERSION
 * @author gweinbach on 15/06/2022
 */
trait Plane[T: Numeric : Precision]:

  private val _Numeric = summon[Numeric[T]]

  private val _0: T = _Numeric.zero
  private val __1: T = _Numeric.one // double '_' to avoid conflict with Product<X>._1

  type Object2D = PlanePoint | PlaneVector

  case class PlanePoint(x: T,
                        y: T):

    override def equals(obj: Any): Boolean =
      obj match
        case that: PlanePoint if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y)
        case _ =>
          false

  object PlanePoint:

    val Origin: PlanePoint = PlanePoint(_0, _0)

  case class PlaneVector(x: T,
                         y: T):

    inline def unary_- =
      PlaneVector(-x, -y)

    lazy val inverse: Option[PlaneVector] =
      if (x ~= _0) || (y ~= _0) then
        None
      else
        Some(PlaneVector(__1 / x, __1 / y))

    override def equals(obj: Any): Boolean =
      obj match
        case that: PlaneVector if (that != null) =>
          (this.x ~= that.x) &&
            (this.y ~= that.y)
        case _ =>
          false

  object PlaneVector:

    def apply(p1: PlanePoint,
              p2: PlanePoint): PlaneVector =
      new PlaneVector(
        p2.x - p1.x,
        p2.y - p1.y
      )

    def apply(p2: PlanePoint): PlaneVector =
      apply(PlanePoint.Origin, p2)

    def fill(t: T): PlaneVector =
      PlaneVector(t, t)


  case class ClippingWindow(width: T,
                            height: T):
    override def equals(obj: Any): Boolean =
      obj match
        case that: ClippingWindow if (that != null) =>
          (this.width ~= that.width) &&
            (this.height ~= that.height)
        case _ =>
          false


  case class ViewPort(width: Int,
                      height: Int)

