/*
 * @author gweinbach on 19/06/2022 12:01
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.component

import spire.algebra.Trig

/**
 * @since 0.2.0
 * @author gweinbach on 19/06/2022
 */
trait Solids[D: Dimension: Trig]
  extends Mobiles[D]
  with AngularAccelerations[D]:

  import CoordsDimension.*
  import CoordsDimension.Ez3D.*
  
  case class Solid(mobile: Mobile,
                   basis: Basis,
                   spin: Spin,
                   spinRange: SpinRange,
                   angularAcceleration: AngularAcceleration,
                   angularAccelerationRange: AngularAccelerationRange):
  
    def move: Solid =
      copy(mobile = mobile.move)
  
    def accelerate: Solid =
      copy(mobile = mobile.accelerate)
  
    def turn(newAcceleration: Acceleration): Solid =
      copy(mobile = mobile.turn(newAcceleration))
  
    def rotate: Solid =
      copy(basis = spin.rotate(basis))
  
    def angularAccelerate: Solid =
      copy(spin = angularAcceleration.accelerate(spin, within = spinRange))
  
    def twirl(newAngularAcceleration: AngularAcceleration): Solid =
      copy(angularAcceleration = newAngularAcceleration.truncate(within = angularAccelerationRange))
      
    def withMobile(mobile: Mobile): Solid =
      copy(mobile = mobile)
  
  case class SolidConfig(mobileConfig: MobileConfig,
                         spinRange: SpinRange,
                         angularAccelerationRange: AngularAccelerationRange)
