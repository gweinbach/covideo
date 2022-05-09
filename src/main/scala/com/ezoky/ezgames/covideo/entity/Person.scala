/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.{Acceleration, Area, HealthCondition, Position, Speed, Sprite}


/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Person(position: Position,
                  speed: Speed,
                  healthCondition: HealthCondition,
                  sprite: Sprite):

  def move(within: Area): Person =
    val newPosition = speed.move(position, within)
    copy(position = newPosition, sprite = sprite.moveTo(newPosition))
    
  def accelerate(acceleration: Acceleration): Person =
    copy(speed = acceleration.accelerate(speed))
