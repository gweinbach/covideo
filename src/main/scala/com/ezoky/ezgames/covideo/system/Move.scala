/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Pos, Area}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Move(position: Pos, 
                within: Area)
  extends System[Pos] {
  override def evolve: Pos = ???
}
