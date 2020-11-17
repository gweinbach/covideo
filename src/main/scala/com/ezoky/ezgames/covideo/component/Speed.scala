/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import Dimension._

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Speed(value: SpeedValue)

case class XSpeed(value: SpeedValue)
  extends Speed(value)

case class YSpeed(value: SpeedValue)
  extends Speed(value)

