/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension._

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Size(value: SizeValue)

case class Width(value: SizeValue)
  extends Size(value)

case class Height(value: SizeValue)
  extends Size(value)
