package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.*

case class Mobile(position: Position,
                  speed: Speed,
                  speedRange: SpeedRange,
                  acceleration: Acceleration,
                  accelerationRange: AccelerationRange):

  def move(within: Area): Mobile =
    copy(position = speed.move(position, within))

  def accelerate: Mobile =
    copy(speed = acceleration.accelerate(speed, speedRange))

  def turn(newAcceleration: Acceleration): Mobile =
    copy(acceleration = newAcceleration.truncate(within = accelerationRange))


case class MobileConfig(speedRange: SpeedRange,
                        accelerationRange: AccelerationRange)
