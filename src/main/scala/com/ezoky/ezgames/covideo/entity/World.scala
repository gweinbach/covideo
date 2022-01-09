/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.{Area, Scene}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class World[ImageType](area: Area,
                            scene: Scene[ImageType])
