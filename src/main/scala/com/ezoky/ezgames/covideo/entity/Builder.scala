/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *  
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.Generate.Generated

/**
 * @author gweinbach on 30/12/2021
 * @since 0.2.0
 */
trait Builder[T]:

  def build: Generated[T]
