package com.ezoky.ezgames.covideo.component

trait Mobiles[T: Dimension]
  extends Boxes[T]
  with Accelerations[T]:

  import CoordsDimension.*

  case class Mobile(position: Position,
                    area: Box,
                    speed: Speed,
                    speedRange: SpeedRange,
                    acceleration: Acceleration,
                    accelerationRange: AccelerationRange):

    def move: Mobile =
      copy(position = speed.move(position, within = area))

    def accelerate: Mobile =
      copy(speed = acceleration.accelerate(speed, within = speedRange))

    def turn(newAcceleration: Acceleration): Mobile =
      copy(acceleration = newAcceleration.truncate(within = accelerationRange))


  case class MobileConfig(speedRange: SpeedRange,
                          accelerationRange: AccelerationRange)

  given Positioned[Mobile] with
    extension (positioned: Mobile)
      override def position: Position = positioned.position

  given Moving[Mobile] with
    extension (moving: Mobile)
      override def speed: Speed = moving.speed

  given Accelerating[Mobile] with
    extension (accelerating: Mobile)
      override def acceleration: Acceleration = accelerating.acceleration