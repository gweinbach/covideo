/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Position, Area}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Move(position: Position, 
                within: Area)
  extends System[Position] {
  override def evolve: Position = ???
}
