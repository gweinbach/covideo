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
  
  trait Alive extends HealthCondition

  case object Healthy extends Alive

  case object Sick extends Alive

  case object Healed extends Alive

  case object Dead extends HealthCondition
