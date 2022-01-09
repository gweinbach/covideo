/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.{HealthCondition, Movement, Position, Sprite}


/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Person[ImageType](position: Position,
                             movement: Movement,
                             healthCondition: HealthCondition,
                             sprite: Sprite[ImageType])
