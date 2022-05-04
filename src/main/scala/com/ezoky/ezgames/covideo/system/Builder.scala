package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Area, Generated}

/**
 * @author gweinbach on 30/12/2021
 * @since 0.2.0
 */
trait Builder[T]:

  def build: Generated[T]


