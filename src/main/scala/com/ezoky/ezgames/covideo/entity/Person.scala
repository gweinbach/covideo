/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.{HealthCondition, Position3D} 

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Person(position: Position3D,
                  healthCondition: HealthCondition)
