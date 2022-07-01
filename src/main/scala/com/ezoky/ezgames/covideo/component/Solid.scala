/*
 * @author gweinbach on 19/06/2022 12:01
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ez3d.Transformation3D
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*

/**
 * @since 0.2.0
 * @author gweinbach on 19/06/2022
 */
case class Solid(mobile: Mobile,
                 basis: Basis):

  def move: Solid =
    copy(mobile = mobile.move)

  def accelerate: Solid =
    copy(mobile = mobile.accelerate)

  def turn(newAcceleration: Acceleration): Solid =
    copy(mobile = mobile.turn(newAcceleration))

  def spin(newBasis: Basis): Solid =
    copy(basis = newBasis)

case class SolidConfig(mobileConfig: MobileConfig)
