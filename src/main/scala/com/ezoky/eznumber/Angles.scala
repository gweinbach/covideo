package com.ezoky.eznumber

import spire.algebra.Trig
import spire.math.Numeric

import scala.annotation.targetName
import scala.reflect.Typeable

/**
 * @since 0.2.0
 * @author gweinbach on 12/06/2022
 */
trait AngleTypes[T: Trig: Numeric]:

  opaque type Degrees = T
  opaque type Radians = T

  final type Angle = Radians | Degrees

  extension (t: T)
    def degrees: Degrees = t
    def radians: Radians = t

  private val _trig = summon[Trig[T]]
  private val _num = summon[Numeric[T]]

  val AngleIsNumeric: Numeric[Angle] = _num
  val RadianIsNumeric: Numeric[Radians] = _num
  val DegreeIsNumeric: Numeric[Degrees] = _num

  sealed trait UniformTrig[A <: Angle: Typeable]:
    def pi: A

    def sin(a: A): T
    def cos(a: A): T
    def tan(a: A): T

    def asin(t: T): A
    def acos(t: T): A
    def atan(t: T): A
    def atan2(y: T, x: T): A

    def toRadians(a: A): Radians
    def toDegrees(a: A): Degrees

  given UniformTrig[Radians] with
    def pi: Radians = _trig.pi

    def sin(a: Radians): T = _trig.sin(a)
    def cos(a: Radians): T = _trig.cos(a)
    def tan(a: Radians): T = _trig.tan(a)

    def asin(t: T): Radians = _trig.asin(t)
    def acos(t: T): Radians = _trig.acos(t)
    def atan(t: T): Radians = _trig.atan(t)
    def atan2(y: T, x: T): Radians = _trig.atan2(x, y)

    def toRadians(a: Radians): Radians = a
    def toDegrees(a: Radians): Degrees = _trig.toDegrees(a)


  given UniformTrig[Degrees] with
    def pi: Degrees = _trig.toDegrees(_trig.pi)

    def sin(a: Degrees): T = _trig.sin(_trig.toRadians(a))
    def cos(a: Degrees): T = _trig.cos(_trig.toRadians(a))
    def tan(a: Degrees): T = _trig.tan(_trig.toRadians(a))

    def asin(t: T): Degrees = _trig.toDegrees(_trig.asin(t))
    def acos(t: T): Degrees = _trig.toDegrees(_trig.acos(t))
    def atan(t: T): Degrees = _trig.toDegrees(_trig.atan(t))
    def atan2(y: T, x: T): Degrees = _trig.toDegrees(_trig.atan2(x, y))

    def toRadians(a: Degrees): Radians = _trig.toRadians(a)
    def toDegrees(a: Degrees): Degrees = a


trait Angles[T: Trig: Numeric] extends AngleTypes[T]:

  given Numeric[Angle] = AngleIsNumeric
  given Numeric[Radians] = RadianIsNumeric
  given Numeric[Degrees] = DegreeIsNumeric

  def pi[U <: Angle: UniformTrig]: U =
    summon[UniformTrig[U]].pi

  def sin[U <: Angle: UniformTrig](a: U): T =
    summon[UniformTrig[U]].sin(a)

  def cos[U <: Angle: UniformTrig](a: U): T =
    summon[UniformTrig[U]].cos(a)

  def tan[U <: Angle: UniformTrig](a: U): T =
    summon[UniformTrig[U]].tan(a)

  def asin[U <: Angle: UniformTrig](t: T): U =
    summon[UniformTrig[U]].asin(t)

  def acos[U <: Angle: UniformTrig](t: T): U =
    summon[UniformTrig[U]].acos(t)

  def atan[U <: Angle: UniformTrig](t: T): U =
    summon[UniformTrig[U]].atan(t)

  def atan2[U <: Angle: UniformTrig](y: T, x: T): U =
    summon[UniformTrig[U]].atan2(x, y)

  def toRadians[U <: Angle: UniformTrig](a: U): Radians =
    summon[UniformTrig[U]].toRadians(a)

  def toDegrees[U <: Angle: UniformTrig](a: U): Degrees =
    summon[UniformTrig[U]].toDegrees(a)

  given [U <: Angle: UniformTrig]: Conversion[U, Radians] with
    def apply(a: U): Radians = a.toRadians

  given [U <: Angle: UniformTrig]: Conversion[U, Degrees] with
    def apply(a: U): Degrees = a.toDegrees


  extension [U <: Angle: UniformTrig](a: U)
    @targetName("convertAngleToRadians")
    def toRadians: Radians =
      summon[UniformTrig[U]].toRadians(a)

    @targetName("convertAngleToDegrees")
    def toDegrees: Degrees =
      summon[UniformTrig[U]].toDegrees(a)

//  extension [U <: Angle: Precision](a1: U)
//    infix def ~=(a2: U): Boolean =
//      summon[Precision[U]].~=(a1, a2)


  private val _trig = summon[Trig[T]]

//  given UniformTrig[Angle] with
//    def pi: Angle = _trig.pi radians
//
//    def sin(a: Angle): T =
//      a match
//        case r: Radians => sin(r)
//        case d: Degrees => sin(d)
//
//    def cos(a: Angle): T =
//      a match
//        case r: Radians => cos(r)
//        case d: Degrees => cos(d)
//
//    def tan(a: Angle): T =
//      a match
//        case r: Radians => tan(r)
//        case d: Degrees => tan(d)
//
//    def asin(t: T): Angle = (_trig.asin(t) radians)
//    def acos(t: T): Angle = (_trig.acos(t) radians)
//    def atan(t: T): Angle = (_trig.atan(t) radians)
//    def atan2(y: T, x: T): Angle = (_trig.atan2(x, y) radians)
//
//    def toRadians(a: Angle): Radians =
//      a match
//        case r: Radians => r
//        case d: Degrees => toRadians(d)
//
//    def toDegrees(a: Angle): Degrees =
//      a match
//        case r: Radians => toDegrees(r)
//        case d: Degrees => d
//
  