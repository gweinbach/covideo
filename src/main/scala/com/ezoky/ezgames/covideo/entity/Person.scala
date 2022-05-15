/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.{Acceleration, AccelerationRange, Area, HealthCondition, Position, Speed, SpeedRange, Sprite}


/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Person(position: Position,
                  speed: Speed,
                  speedRange: SpeedRange,
                  acceleration: Acceleration,
                  accelerationRange: AccelerationRange,
                  healthCondition: HealthCondition,
                  sprite: Sprite):

  def move(within: Area): Person =
    val newPosition = speed.move(position, within)
    copy(position = newPosition, sprite = sprite.moveTo(newPosition))

  def accelerate: Person =
    copy(speed = acceleration.accelerate(speed, speedRange))

  def turn(newAcceleration: Acceleration): Person =
    copy(acceleration = newAcceleration.truncate(within = accelerationRange))


case class PersonConfig(initialSpeedRange: SpeedRange,
                        accelerationRange: AccelerationRange)
