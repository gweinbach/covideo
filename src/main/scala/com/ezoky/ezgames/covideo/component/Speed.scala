/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import Dimension._

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Speed(value: MovementValue)

case class XSpeed(value: MovementValue)
  extends Speed(value)

case class YSpeed(value: MovementValue)
  extends Speed(value)

