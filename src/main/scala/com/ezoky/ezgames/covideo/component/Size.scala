/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension._

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
enum Size(value: SizeValue) {

  case Width(value: SizeValue)
    extends Size(value)

  case Height(value: SizeValue)
    extends Size(value)
  
  case Depth(value: SizeValue)
    extends Size(value)

}
