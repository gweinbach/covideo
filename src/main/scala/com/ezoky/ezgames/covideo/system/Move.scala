/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Area, Position3D}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Move(position: Position3D,
                within: Area)
  extends System[Position3D] {
  override def evolve: Position3D = ???
}
