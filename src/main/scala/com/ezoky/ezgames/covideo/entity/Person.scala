/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.{HealthCondition, Position}


/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Person(position: Position,
                  healthCondition: HealthCondition)
