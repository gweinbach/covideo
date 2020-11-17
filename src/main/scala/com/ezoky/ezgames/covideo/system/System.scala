/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.system

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait System[T] {

  def evolve: T
}
