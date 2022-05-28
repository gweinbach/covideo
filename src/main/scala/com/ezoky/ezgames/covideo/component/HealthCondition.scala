/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait HealthCondition 

object HealthCondition:

  case object Healthy extends HealthCondition

  case object Sick extends HealthCondition

  case object Healed extends HealthCondition
