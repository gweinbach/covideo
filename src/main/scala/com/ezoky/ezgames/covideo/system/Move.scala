/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Area, Movement, Position}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Move(position: Position,
                movement: Movement,
                within: Area)
  extends System[Position] {
  override def evolve: Position =
    movement.move(position, within)
}
